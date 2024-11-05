import { SavedPlannerYearData } from '@peterportal/types';
import { eq, or, sql } from 'drizzle-orm';
import {
  boolean,
  check,
  index,
  integer,
  jsonb,
  pgTable,
  primaryKey,
  real,
  text,
  timestamp,
  unique,
} from 'drizzle-orm/pg-core';

export const user = pgTable(
  'user',
  {
    id: integer('id').primaryKey().generatedAlwaysAsIdentity(),
    googleId: text('google_id').notNull(),
    name: text('name').notNull(),
    email: text('email').notNull(),
    picture: text('picture').notNull(),
    theme: text('theme'),
    lastRoadmapEditAt: timestamp('last_roadmap_edit_at'),
  },
  (table) => ({
    uniqueGoogleId: unique('unique_google_id').on(table.googleId),
  }),
);

export const report = pgTable(
  'report',
  {
    id: integer('id').primaryKey().generatedAlwaysAsIdentity(),
    reviewId: integer('review_id')
      .notNull()
      .references(() => review.id, { onDelete: 'cascade' }),
    reason: text('reason').notNull(),
    createdAt: timestamp('created_at').defaultNow().notNull(),
  },
  (table) => ({
    reviewIdIdx: index('reports_review_id_idx').on(table.reviewId),
  }),
);

export const review = pgTable(
  'review',
  {
    id: integer('id').primaryKey().generatedAlwaysAsIdentity(),
    professorId: text('professor_id').notNull(),
    courseId: text('course_id').notNull(),
    userId: integer('user_id')
      .notNull()
      .references(() => user.id),
    anonymous: boolean('anonymous').notNull(),
    content: text('content'),
    rating: integer('rating').notNull(),
    difficulty: integer('difficulty').notNull(),
    gradeReceived: text('grade_received').notNull(),
    createdAt: timestamp('created_at').defaultNow().notNull(),
    updatedAt: timestamp('updated_at').defaultNow(),
    forCredit: boolean('for_credit').notNull(),
    quarter: text('quarter').notNull(),
    takeAgain: boolean('take_again').notNull(),
    textbook: boolean('textbook').notNull(),
    attendance: boolean('attendance').notNull(),
    tags: text('tags').array(),
    verified: boolean('verified').notNull().default(false),
  },
  (table) => ({
    ratingCheck: check('rating_check', sql`${table.rating} >= 1 AND ${table.rating} <= 5`),
    difficultyCheck: check('difficulty_check', sql`${table.difficulty} >= 1 AND ${table.difficulty} <= 5`),
    unique: unique('unique_review').on(table.userId, table.professorId, table.courseId),
    professorIdIdx: index('reviews_professor_id_idx').on(table.professorId),
    courseIdIdx: index('reviews_course_id_idx').on(table.courseId),
  }),
);

export const planner = pgTable(
  'planner',
  {
    id: integer('id').primaryKey().generatedAlwaysAsIdentity(),
    userId: integer('user_id').references(() => user.id),
    name: text('name').notNull(),
    years: jsonb('years').$type<SavedPlannerYearData>().array().notNull(),
  },
  (table) => ({
    userIdIdx: index('planners_user_id_idx').on(table.userId),
  }),
);

export const transferredCourse = pgTable(
  'transferred_course',
  {
    userId: integer('user_id').references(() => user.id),
    courseName: text('course_name'),
    units: real('units'),
  },
  (table) => ({
    userIdIdx: index('transferred_courses_user_id_idx').on(table.userId),
  }),
);

export const vote = pgTable(
  'vote',
  {
    reviewId: integer('review_id')
      .notNull()
      .references(() => review.id, { onDelete: 'cascade' }),
    userId: integer('user_id')
      .notNull()
      .references(() => user.id),
    vote: integer('vote').notNull(),
  },
  (table) => ({
    voteCheck: check('votes_vote_check', or(eq(table.vote, 1), eq(table.vote, -1))!),
    primaryKey: primaryKey({ columns: [table.reviewId, table.userId] }),
    userIdIdx: index('votes_user_id_idx').on(table.userId),
  }),
);

export const savedCourse = pgTable(
  'saved_course',
  {
    userId: integer('user_id')
      .references(() => user.id)
      .notNull(),
    courseId: text('course_id').notNull(),
  },
  (table) => ({
    primaryKey: primaryKey({ columns: [table.userId, table.courseId] }),
  }),
);

export const session = pgTable('session', {
  sid: text('sid').primaryKey(),
  sess: jsonb('sess').notNull(),
  expire: timestamp('expire').notNull(),
});
