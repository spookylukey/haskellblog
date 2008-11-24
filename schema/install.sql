
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

