'use strict';

module.exports = {
  client: 'pg',
  connection: process.env.DATABASE_URL || 'postgresql://postgres:password@localhost:5432/octane'
};
