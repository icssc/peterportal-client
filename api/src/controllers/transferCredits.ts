import { router, publicProcedure } from '../helpers/trpc';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';
import { z } from 'zod';
import { db } from '../db';
import { transferredApExam } from '../db/schema';
import { eq } from 'drizzle-orm';
import { APExam } from '@peterportal/types';

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
  /** @todo add user procedure to get transferred AP Exams below this comment. */

  getAPExamInfo: publicProcedure.query(async (): Promise<APExam[]> => {
    const response = await fetch(`${process.env.PUBLIC_API_URL}apExams`, {
      headers: ANTEATER_API_REQUEST_HEADERS,
    })
      .then((res) => res.json())
      .then((res) => (res.data ? (res.data as APExam[]) : []));
    return response;
  }),
  // only update one row at a time
  getSavedAPExams: publicProcedure.query(async ({ ctx }): Promise<userAPExam[]> => {
    /** you should return the data as-is, then handle the null case from the frontend within an individual APExamTile */
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
  saveAPExam: publicProcedure.input(zodAPExamSchema).mutation(async ({ input, ctx }) => {
    const { examName, score, units } = input;
    const userId = ctx.session.userId!;

    const rowToInsert = {
      userId,
      examName,
      score: score ?? null,
      units,
    };
    await db.insert(transferredApExam).values(rowToInsert);
  }),
  deleteUserAPExam: publicProcedure.input(z.string()).mutation(async ({ input, ctx }) => {
    const examName = input;
    const userId = ctx.session.userId!;
    // await db.delete(transferredApExam).where(eq(transferredApExam.userId, userId));

    await db
      .delete(transferredApExam)
      .where(eq(transferredApExam.userId, userId) && eq(transferredApExam.examName, examName));
  }),
  updateUserAPExam: publicProcedure.input(zodAPExamSchema).mutation(async ({ input, ctx }) => {
    const { examName, score, units } = input;
    const userId = ctx.session.userId!;
    // await db.delete(transferredApExam).where(eq(transferredApExam.userId, userId));

    await db
      .update(transferredApExam)
      .set({ score: score, units: units })
      .where(eq(transferredApExam.userId, userId) && eq(transferredApExam.examName, examName));
  }),
  /** @todo add user procedure to get transferred GE credits below this comment. */
  /** @todo add user procedure to get transferred untransferred credits below this comment. */
});

export default transferCreditsRouter;
