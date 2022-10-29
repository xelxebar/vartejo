#!/usr/bin/env bash
set -o errexit -o noclobber -o nounset
# trap 'tput setaf 1; tput bold; echo $BASH_COMMAND; read; tput sgr0' DEBUG
trap 'echo "### $BASH_COMMAND"' DEBUG

guix_key_url='https://sv.gnu.org/people/viewgpg.php?user_id=127547'
guix_bin_url='https://ftp.gnu.org/gnu/guix'
guix_bin_version=1.3.0
guix_bin_arch=x86_64

### Install dependencies
apt-get update
apt-get install --assume-yes curl gnupg xz-utils

### Download Guix Binary
curl --location "$guix_key_url" | gpg --import -
curl --location \
	--output guix.tar.xz "$guix_bin_url/guix-binary-$guix_bin_version.$guix_bin_arch-linux.tar.xz" \
	--output guix.tar.xz.sig "$guix_bin_url/guix-binary-$guix_bin_version.$guix_bin_arch-linux.tar.xz.sig"
gpg --verify 'guix.tar.xz.sig' 'guix.tar.xz'

### Setup Guix Daemon
tar --directory=/tmp --warning=no-timestamp --extract --xz --verbose \
	--file="guix.tar.xz"
mv /tmp/var/guix /var
mv /tmp/gnu /
mkdir --parent ~root/.config/guix
ln --symbolic --force /var/guix/profiles/per-user/root/current-guix \
	~root/.config/guix/current

groupadd --system guixbuild
for i in {01..10}; do
	useradd --gid guixbuild --groups guixbuild \
		--no-create-home --shell "$(command -v nologin)" \
		--system --comment "Guix build user $i" \
		"guixbuilder$i"
done

cp ~root/.config/guix/current/lib/systemd/system/guix-daemon.service \
	/etc/systemd/system
systemctl start guix-daemon
systemctl enable guix-daemon

### Setup Temporary Environment
export GUIX_PROFILE=~root/.config/guix/current
#shellcheck disable=SC1091
. "$GUIX_PROFILE/etc/profile"

mkdir --parent /usr/local/bin
ln --symbolic --target-directory=/usr/local/bin \
	/var/guix/profiles/per-user/root/current-guix/bin/guix

mkdir --parent /usr/local/share/info
ln --symbolic --target-directory=/usr/local/share/info \
	/var/guix/profiles/per-user/root/current-guix/share/info/*

### Update Package Database
mkdir -p ~/.config/guix
cat >~/.config/guix/channels.scm <<-CHANNELS_SCM
	(cons* (channel
	        (name 'nonguix)
	        (url "https://gitlab.com/nonguix/nonguix")
	        ;; Enable signature verification:
	        (introduction
	         (make-channel-introduction
	          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
	          (openpgp-fingerprint
	           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
	       %default-channels)
CHANNELS_SCM

guix archive --authorize <~root/.config/guix/current/share/guix/ci.guix.gnu.org.pub
curl --location 'https://substitutes.nonguix.org/signing-key.pub' \
	| guix archive --authorize

guix pull

### Install Dependencies Needed for guix system reconfigure
guix package --install glibc-locales
export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
guix package --install openssl

### Create Bootstrap Configuration
dev_root=$(awk '$2 == "/" { print $1 }' /proc/mounts)
ip=$(ip -brief -family inet address show dev eth0 | awk '{print $3}')
gateway=$(ip route show default | awk '{print $3}')
cat >/etc/bootstrap-config.scm <<-CONFIG_SCM
	(use-modules (gnu)
	             (nongnu packages linux)
	             (nongnu system linux-initrd)))
	(use-service-modules networking ssh)

	(operating-system
	  (host-name "gnu-bootstrap")
	  (timezone "Etc/UTC")

	  (bootloader (bootloader-configuration
	               (bootloader grub-bootloader)
	               (targets '("/dev/null"))))

	  (kernel linux)
	  (initrd microcode-initrd)

	  (file-systems (cons (file-system
	                        (mount-point "/")
	                        (device "$dev_root")
	                        (type "xfs"))
	                      %base-file-systems))

	  (services
	    (append
	      (list (service static-networking-service-type
	                     (list (static-networking
	                            (addresses
	                             (list (network-address
	                                    (device "eth0")
	                                    (value "$ip"))))
	                            (routes
	                             (list (network-route
	                                    (destination "default")
	                                    (gateway "$gateway"))))
	                            (name-servers '("195.190.28.82" "195.190.28.85"))))
	            (simple-service 'guile-load-path-in-global-env
	             session-environment-service-type
	             (quasiquote
	               (("GUILE_LOAD_PATH"
	                 . "/run/current-system/profile/share/guile/site/3.0")
	                ("GUILE_LOAD_COMPILED_PATH"
	                 . ,(string-join '("/run/current-system/profile/lib/guile/3.0/site-ccache"
	                                   "/run/current-system/profile/share/guile/site/3.0")
	                                 ":"))))
	            (service openssh-service-type
	              (openssh-configuration
	               (log-level 'debug)
	               (permit-root-login 'prohibit-password))))
	      %base-services)))
CONFIG_SCM

exit

### Bootstrap
guix system build /etc/bootstrap-config.scm
guix system reconfigure /etc/bootstrap-config.scm # ???
# guix pull
#shellcheck disable=SC1091
# . "$GUIX_PROFILE/etc/profile"
mv /etc /etc.old
mkdir /etc
cp --recursive \
	/etc.old/{passwd,group,shadow,gshadow,mtab} \
	/etc.old/{guix,bootstrap-config.scm,resolv.conf,services,hosts} \
	/etc
# guix system init --no-bootloader /etc/bootstrap-config.scm /
guix system reconfigure /etc/bootstrap-config.scm
