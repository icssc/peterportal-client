import { z } from 'zod';
import { TransferredGE } from '@peterportal/types';
import { router, publicProcedure, userProcedure } from '../helpers/trpc';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';
import { db } from '../db';
import { transferredApExam, transferredGe } from '../db/schema';
import { APExam } from '@peterportal/types';
import { and, eq, isNull } from 'drizzle-orm';
import { transferredCourse } from '../db/schema';
import { transferredMisc } from '../db/schema';

interface userAPExam {
  examName: string;
  score: number;
  units: number;
}

const zodAPExamSchema = z.object({
  examName: z.string(),
  score: z.number(),
  units: z.number(),
});

/** @todo complete all routes. We will remove comments after all individual PRs are merged to avoid merge conflicts */
const transferCreditsRouter = router({
  /** @todo add user procedure to get transferred courses below this comment. */
  getTransferredCourses: userProcedure.query(async ({ ctx }) => {
    const response = await db
      .select({ courseName: transferredCourse.courseName, units: transferredCourse.units })
      .from(transferredCourse)
      .where(eq(transferredCourse.userId, ctx.session.userId!));
    return response;
  }),
  addTransferredCourse: userProcedure
    .input(z.object({ courseName: z.string(), units: z.number() }))
    .mutation(async ({ ctx, input }) => {
      await db
        .insert(transferredCourse)
        .values({ courseName: input.courseName, units: input.units, userId: ctx.session.userId! });
    }),
  removeTransferredCourse: userProcedure.input(z.string()).mutation(async ({ ctx, input }) => {
    await db
      .delete(transferredCourse)
      .where(and(eq(transferredCourse.userId, ctx.session.userId!), eq(transferredCourse.courseName, input)));
  }),
  updateTransferredCourse: userProcedure
    .input(z.object({ courseName: z.string(), units: z.number() }))
    .mutation(async ({ ctx, input }) => {
      await db
        .update(transferredCourse)
        .set({ units: input.units })
        .where(
          and(eq(transferredCourse.userId, ctx.session.userId!), eq(transferredCourse.courseName, input.courseName)),
        );
    }),
  /** @todo add user procedure to get transferred AP Exams below this comment. */
  getAPExamInfo: publicProcedure.query(async (): Promise<APExam[]> => {
    const response = await fetch(`${process.env.PUBLIC_API_URL}apExams`, {
      headers: ANTEATER_API_REQUEST_HEADERS,
    })
      .then((res) => res.json())
      .then((res) => (res.data ? (res.data as APExam[]) : []));
    return response;
  }),
  getSavedAPExams: publicProcedure.query(async ({ ctx }): Promise<userAPExam[]> => {
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
  addUserAPExam: publicProcedure.input(zodAPExamSchema).mutation(async ({ input, ctx }) => {
    const { examName, score, units } = input;
    const userId = ctx.session.userId!;

    await db.insert(transferredApExam).values({ userId, examName, score: score ?? null, units });
  }),
  deleteUserAPExam: publicProcedure.input(z.string()).mutation(async ({ input, ctx }) => {
    const examName = input;
    const userId = ctx.session.userId!;

    await db
      .delete(transferredApExam)
      .where(eq(transferredApExam.userId, userId) && eq(transferredApExam.examName, examName));
  }),
  updateUserAPExam: publicProcedure.input(zodAPExamSchema).mutation(async ({ input, ctx }) => {
    const { examName, score, units } = input;
    const userId = ctx.session.userId!;

    await db
      .update(transferredApExam)
      .set({ score: score, units: units })
      .where(eq(transferredApExam.userId, userId) && eq(transferredApExam.examName, examName));
  }),
  /** @todo add user procedure to get transferred GE credits below this comment. */
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
  setTransferredGE: userProcedure
    .input(
      z.object({
        GE: z.object({
          geName: z.string(),
          numberOfCourses: z.number(),
          units: z.number(),
        }),
      }),
    )
    .mutation(async ({ input, ctx }) => {
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
  /** @todo add user procedure to get transferred untransferred credits below this comment. */
  getUncategorizedTransfers: userProcedure.query(async ({ ctx }) => {
    const courses = await db
      .select({ name: transferredMisc.courseName, units: transferredMisc.units })
      .from(transferredMisc)
      .where(eq(transferredMisc.userId, ctx.session.userId!));
    return courses;
  }),

  removeUncategorizedCourse: userProcedure
    .input(z.object({ name: z.string().nullable(), units: z.number().nullable() }))
    .mutation(async ({ ctx, input }) => {
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
});

export default transferCreditsRouter;
