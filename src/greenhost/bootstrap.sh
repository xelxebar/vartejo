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
guix archive --authorize <~root/.config/guix/current/share/guix/ci.guix.gnu.org.pub
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
	(use-modules (gnu))
	(use-package-modules certs tmux)
	(use-service-modules networking ssh)

	(operating-system
	  (host-name "gnu-bootstrap")
	  (timezone "Etc/UTC")

	  (bootloader (bootloader-configuration
	               (bootloader grub-bootloader)
	               (targets '("/dev/null"))))

	  (file-systems (cons (file-system
	                        (mount-point "/")
	                        (device "$dev_root")
	                        (type "xfs"))
	                      %base-file-systems))

	  (packages (list nss-certs mosh tmux))

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
	                            (name-servers '("195.190.28.82" "195.190.28.85")))))
	            (service openssh-service-type
	              (openssh-configuration
	               (log-level 'debug)
	               (permit-root-login 'prohibit-password))))
	      %base-services)))
CONFIG_SCM
