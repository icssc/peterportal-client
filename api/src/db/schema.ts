import { sql } from 'drizzle-orm';
import { boolean, check, integer, jsonb, pgTable, timestamp, varchar } from 'drizzle-orm/pg-core';

export const users = pgTable('users', {
  id: integer().primaryKey(),
  displayName: varchar({ length: 255 }).notNull(),
  theme: varchar({ length: 8 }),
  lastRoadmapEditAt: timestamp(),
});

export const reports = pgTable('reports', {
  id: integer().primaryKey().generatedAlwaysAsIdentity(),
  reason: varchar({ length: 500 }).notNull(),
  createdAt: timestamp().defaultNow().notNull(),
});

export const reviews = pgTable(
  'reviews',
  {
    id: integer().primaryKey().generatedAlwaysAsIdentity(),
    professorId: varchar({ length: 8 }).notNull(),
    courseId: varchar({ length: 16 }).notNull(),
    userId: integer()
      .notNull()
      .references(() => users.id),
    anonymous: boolean().notNull(),
    content: varchar({ length: 500 }).notNull(),
    rating: integer().notNull(),
    difficulty: integer().notNull(),
    createdAt: timestamp().defaultNow().notNull(),
    updatedAt: timestamp().defaultNow().notNull(),
    forCredit: boolean().notNull(),
    quarter: varchar({ length: 32 }).notNull(),
    score: integer().notNull() /** @todo: could do a count query on votes instead? */,
    takeAgain: boolean().notNull(),
    textbook: boolean().notNull(),
    attendance: boolean().notNull(),
    tags: varchar({ length: 255 }).array(),
    verified: boolean().notNull().default(false),
  },
  (table) => ({
    ratingCheck: check('rating_check', sql`${table.rating} >= 1 AND ${table.rating} <= 5`),
    difficultyCheck: check('difficulty_check', sql`${table.difficulty} >= 1 AND ${table.difficulty} <= 5`),
  }),
);

export const planners = pgTable('planners', {
  userId: integer().references(() => users.id),
  planner: jsonb(),
});

export const transferredCourses = pgTable('transferredCourses', {
  userId: integer().references(() => users.id),
  courseName: varchar({ length: 32 }),
  units: integer(),
});

export const votes = pgTable(
  'votes',
  {
    id: integer().primaryKey().generatedAlwaysAsIdentity(),
    reviewId: integer()
      .notNull()
      .references(() => reviews.id),
    userId: integer()
      .notNull()
      .references(() => users.id),
    vote: integer().notNull(),
  },
  (table) => ({
    voteCheck: check('vote_check', sql`${table.vote} = 1 OR ${table.vote} == -1`),
  }),
);
