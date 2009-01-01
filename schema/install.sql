
  CREATE TABLE metainfo (
    key TEXT,
    value TEXT
  );

  CREATE TABLE formats (
    id INTEGER PRIMARY KEY,
    name TEXT,
    posts_enabled INTEGER,
    comments_enabled INTEGER
  );

  CREATE TABLE posts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    title TEXT,
    slug TEXT,
    post_raw TEXT,
    post_formatted TEXT,
    summary_raw TEXT,
    summary_formatted TEXT,
    format_id INTEGER REFERENCES formats(id),
    timestamp INTEGER,
    comments_open INTEGER
  );

  CREATE TABLE categories (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT,
    slug TEXT
  );

  CREATE TABLE post_categories (
    post_id INTEGER REFERENCES posts(id),
    category_id INTEGER REFERENCES categories(id)
  );

  CREATE TABLE comments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    post_id INTEGER REFERENCES posts(id),
    timestamp INTEGER,
    name TEXT,
    email TEXT,
    text_raw TEXT,
    text_formatted TEXT,
    format_id INTEGER REFERENCES format(id)
  );

CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT,
    password TEXT,
    superuser INTEGER
);

CREATE UNIQUE INDEX post_slug_index ON posts (slug);
CREATE UNIQUE INDEX post_id_index ON posts (id);
CREATE INDEX post_ts_index ON posts (timestamp);
CREATE UNIQUE INDEX category_id_index ON categories (id);
CREATE UNIQUE INDEX category_slug_index ON categories (slug);
CREATE INDEX postcat_pid_index ON post_categories (post_id);
CREATE INDEX postcat_cid_index ON post_categories (category_id);
CREATE UNIQUE INDEX comment_id_index ON comments (id);
CREATE INDEX comment_pid_index ON comments (post_id);
CREATE INDEX comment_ts_index ON comments (timestamp);
CREATE UNIQUE INDEX user_username_index ON users (username);
