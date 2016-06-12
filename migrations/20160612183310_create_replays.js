'use strict';

module.exports = {
  up: (knex) => {
    return knex.schema.createTable('replays', (table) => {
      table.timestamp('created_at').defaultTo(knex.raw('now()'));
      table.uuid('guid').notNullable().primary();
      table.string('map_name').notNullable();
      table.string('match_type').notNullable();
      table.integer('num_frames').notNullable();
      table.date('played_on').notNullable();
      table.integer('team_0_score').notNullable();
      table.integer('team_1_score').notNullable();
      table.integer('team_size').notNullable();
    });
  },

  down: (knex) => {
    return knex.schema.dropTable('replays');
  }
};
