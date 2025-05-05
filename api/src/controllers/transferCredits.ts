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
  apExams: z.array(
    z.object({
      examName: z.string(),
      score: z.number(),
      units: z.number(),
    }),
  ),
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
  saveSelectedAPExams: publicProcedure.input(zodAPExamSchema).mutation(async ({ input, ctx }) => {
    const { apExams } = input;
    const userId = ctx.session.userId!;
    await db.delete(transferredApExam).where(eq(transferredApExam.userId, userId));

    const rowsToInsert = apExams.map((exam) => ({
      userId,
      examName: exam.examName,
      score: exam.score,
      units: exam.units,
    }));
    if (rowsToInsert.length) await db.insert(transferredApExam).values(rowsToInsert);
  }),
  /** @todo add user procedure to get transferred GE credits below this comment. */
  /** @todo add user procedure to get transferred untransferred credits below this comment. */
});

export default transferCreditsRouter;
