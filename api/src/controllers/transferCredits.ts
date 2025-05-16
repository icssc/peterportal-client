import { z } from 'zod';
import { transferData, TransferredGE } from '@peterportal/types';
import { publicProcedure, router, userProcedure } from '../helpers/trpc';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';
import { db } from '../db';
import { transferredApExam, transferredGe } from '../db/schema';
import { APExam } from '@peterportal/types';
import { and, eq, isNull, sql } from 'drizzle-orm';
import { transferredCourse } from '../db/schema';
import { transferredMisc } from '../db/schema';
import { organizeLegacyTransfers } from '../helpers/transferCredits';

interface userAPExam {
  examName: string;
  score: number;
  units: number;
}

const zodCourseTransferSchema = z.object({
  courseName: z.string(),
  units: z.number(),
});

const zodAPExamSchema = z.object({
  examName: z.string(),
  score: z.number(),
  units: z.number(),
});

const zodGETransferSchema = z.object({
  geName: z.string(),
  numberOfCourses: z.number(),
  units: z.number(),
});

const zodUncategorizedTransferSchema = z.object({
  name: z.string().nullable(),
  units: z.number().nullable(),
});

const transferCreditsRouter = router({
  getTransferredCourses: userProcedure.query(async ({ ctx }) => {
    const response = await db
      .select({ courseName: transferredCourse.courseName, units: transferredCourse.units })
      .from(transferredCourse)
      .where(eq(transferredCourse.userId, ctx.session.userId!));
    return response;
  }),
  addTransferredCourse: userProcedure.input(zodCourseTransferSchema).mutation(async ({ ctx, input }) => {
    await db
      .insert(transferredCourse)
      .values({ courseName: input.courseName, units: input.units, userId: ctx.session.userId! });
  }),
  removeTransferredCourse: userProcedure.input(z.string()).mutation(async ({ ctx, input }) => {
    await db
      .delete(transferredCourse)
      .where(and(eq(transferredCourse.userId, ctx.session.userId!), eq(transferredCourse.courseName, input)));
  }),
  updateTransferredCourse: userProcedure.input(zodCourseTransferSchema).mutation(async ({ ctx, input }) => {
    await db
      .update(transferredCourse)
      .set({ units: input.units })
      .where(
        and(eq(transferredCourse.userId, ctx.session.userId!), eq(transferredCourse.courseName, input.courseName)),
      );
  }),
  getAPExamInfo: publicProcedure.query(async (): Promise<APExam[]> => {
    const response = await fetch(`${process.env.PUBLIC_API_URL}apExams`, {
      headers: ANTEATER_API_REQUEST_HEADERS,
    })
      .then((res) => res.json())
      .then((res) => (res.data ? (res.data as APExam[]) : []));
    return response;
  }),
  getSavedAPExams: userProcedure.query(async ({ ctx }): Promise<userAPExam[]> => {
    const userId = ctx.session.userId;
    if (!userId) return [];

    const res = await db
      .select({ examName: transferredApExam.examName, score: transferredApExam.score, units: transferredApExam.units })
      .from(transferredApExam)
      .where(eq(transferredApExam.userId, userId));
    return res.map((exam) => ({
      examName: exam.examName,
      score: exam.score ?? 0,
      units: exam.units,
    })) as userAPExam[];
  }),
  addUserAPExam: userProcedure.input(zodAPExamSchema).mutation(async ({ input, ctx }) => {
    const { examName, score, units } = input;
    const userId = ctx.session.userId!;

    await db.insert(transferredApExam).values({ userId, examName, score: score ?? null, units });
  }),
  deleteUserAPExam: userProcedure.input(z.string()).mutation(async ({ input, ctx }) => {
    const examName = input;
    const userId = ctx.session.userId!;

    await db
      .delete(transferredApExam)
      .where(eq(transferredApExam.userId, userId) && eq(transferredApExam.examName, examName));
  }),
  updateUserAPExam: userProcedure.input(zodAPExamSchema).mutation(async ({ input, ctx }) => {
    const { examName, score, units } = input;
    const userId = ctx.session.userId!;

    await db
      .update(transferredApExam)
      .set({ score: score, units: units })
      .where(eq(transferredApExam.userId, userId) && eq(transferredApExam.examName, examName));
  }),
  getTransferredGEs: userProcedure.query(async ({ ctx }): Promise<TransferredGE[]> => {
    const response = await db
      .select({
        geName: transferredGe.geName,
        numberOfCourses: transferredGe.numberOfCourses,
        units: transferredGe.units,
      })
      .from(transferredGe)
      .where(eq(transferredGe.userId, ctx.session.userId!));
    return response as TransferredGE[];
  }),
  setTransferredGE: userProcedure.input(z.object({ GE: zodGETransferSchema })).mutation(async ({ input, ctx }) => {
    const { GE } = input as { GE: TransferredGE };
    const userId = ctx.session.userId!;
    await db
      .insert(transferredGe)
      .values({ userId, geName: GE.geName, numberOfCourses: GE.numberOfCourses, units: GE.units })
      .onConflictDoUpdate({
        target: [transferredGe.userId, transferredGe.geName],
        set: { numberOfCourses: GE.numberOfCourses, units: GE.units },
      });
  }),
  getUncategorizedTransfers: userProcedure.query(async ({ ctx }) => {
    const courses = await db
      .select({ name: transferredMisc.courseName, units: transferredMisc.units })
      .from(transferredMisc)
      .where(eq(transferredMisc.userId, ctx.session.userId!));
    return courses;
  }),
  removeUncategorizedCourse: userProcedure.input(zodUncategorizedTransferSchema).mutation(async ({ ctx, input }) => {
    const conditions = [eq(transferredMisc.userId, ctx.session.userId!)];

    if (input.name != null) {
      conditions.push(eq(transferredMisc.courseName, input.name));
    } else {
      conditions.push(isNull(transferredMisc.courseName));
    }

    if (input.units != null) {
      conditions.push(eq(transferredMisc.units, input.units));
    } else {
      conditions.push(isNull(transferredMisc.units));
    }

    await db.delete(transferredMisc).where(and(...conditions));
  }),
  convertUserLegacyTransfers: publicProcedure.input(z.array(transferData)).query(async ({ input }) => {
    return await organizeLegacyTransfers(input);
  }),
  overrideAllTransfers: userProcedure
    .input(
      z.object({
        courses: z.array(zodCourseTransferSchema),
        ap: z.array(zodAPExamSchema),
        ge: z.array(zodGETransferSchema),
        other: z.array(zodUncategorizedTransferSchema),
      }),
    )
    .mutation(async ({ ctx, input }) => {
      const appendUserId = <T extends object>(item: T) => Object.assign(item, { userId: ctx.session.userId! });

      const dbQueries = [];
      if (input.courses.length) {
        const addCoursesOperation = db
          .insert(transferredCourse)
          .values(input.courses.map(appendUserId))
          .onConflictDoUpdate({
            target: [transferredCourse.userId, transferredCourse.courseName],
            set: { units: sql.raw(`EXCLUDED.${transferredCourse.units.name}`) },
          });
        dbQueries.push(addCoursesOperation);
      }
      if (input.ap.length) {
        const addApQuery = db
          .insert(transferredApExam)
          .values(input.ap.map(appendUserId))
          .onConflictDoUpdate({
            target: [transferredApExam.userId, transferredApExam.examName],
            set: {
              score: sql.raw(`EXCLUDED.${transferredApExam.score.name}`),
              units: sql.raw(`EXCLUDED.${transferredApExam.units.name}`),
            },
          });
        dbQueries.push(addApQuery);
      }
      if (input.ge.length) {
        const addGeQuery = db
          .insert(transferredGe)
          .values(input.ge.map(appendUserId))
          .onConflictDoUpdate({
            target: [transferredGe.userId, transferredGe.geName],
            set: {
              numberOfCourses: sql.raw(`EXCLUDED.${transferredGe.numberOfCourses.name}`),
              units: sql.raw(`EXCLUDED.${transferredGe.units.name}`),
            },
          });
        dbQueries.push(addGeQuery);
      }
      if (input.other.length) {
        const rows = input.other.map(({ name: courseName, units }) => ({
          courseName,
          units,
          userId: ctx.session.userId!,
        }));
        const addOtherQuery = db.insert(transferredMisc).values(rows);
        dbQueries.push(addOtherQuery);
      }

      await Promise.all([dbQueries.map((q) => q.execute())]);
    }),
});

export default transferCreditsRouter;
