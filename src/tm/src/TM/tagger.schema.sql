--
-- Copyright © 2016 Brandon Wilson
-- 
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the "Software"),
-- to deal in the Software without restriction, including without limitation
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
-- OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
-- OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


-- | Fundamental schema for tagging

CREATE TABLE [tm:tagger:objs] (
	  id INTEGER PRIMARY KEY
) WITHOUT ROWID;

CREATE TABLE [tm:tagger:tags] (
	  id INTEGER PRIMARY KEY REFERENCES [tm:tagger:tags](id) ON UPDATE CASCADE
	, ptag INTEGER DEFAULT NULL
	, name TEXT NOT NULL
) WITHOUT ROWID;

CREATE TABLE [tm:tagger:tagged] (
	  obj INTEGER NOT NULL REFERENCES [tm:tagger:objs](id) ON UPDATE CASCADE
	, tag TEXT NOT NULL REFERENCES [tm:tagger:tags](id) ON UPDATE CASCADE
	, PRIMARY KEY (obj, tag)
) WITHOUT ROWID;


-- Schema for inter-tag relations

CREATE TABLE [tm:tagger:rels] (
	  id INTEGER PRIMARY KEY
	, name TEXT NOT NULL
) WITHOUT ROWID;

CREATE TABLE [tm:tagger:tagrels] (
	  ltag INTEGER NOT NULL REFERENCES [tm:tagger:tags](id) ON UPDATE CASCADE
	, rtag INTEGER NOT NULL REFERENCES [tm:tagger:tags](id) ON UPDATE CASCADE
	, rel INTEGER NOT NULL REFERENCES [tm:tagger:rels](id) ON UPDATE CASCADE
	, PRIMARY KEY (ltag, rtag, rel)
) WITHOUT ROWID;
