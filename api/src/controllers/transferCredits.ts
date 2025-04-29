import { router, publicProcedure } from '../helpers/trpc';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';

interface APExam {
  fullName: string;
  catalogueName: string;
  rewards: {
    acceptableScores: number[];
    unitsGranted: number;
    electiveUnitsGranted: number;
    geCategories: string[];
    coursesGranted: {
      string: string[];
    };
  }[];
}

const getAPExams = async (): Promise<APExam[]> => {
  const response = await fetch(`${process.env.PUBLIC_API_URL}apExams`, {
    headers: ANTEATER_API_REQUEST_HEADERS,
  })
    .then((res) => res.json())
    .then((res) => (res.data ? (res.data as APExam[]) : []));
  return response;
};

/** @todo complete all routes. We will remove comments after all individual PRs are merged to avoid merge conflicts */
const transferCreditsRouter = router({
  /** @todo add user procedure to get transferred courses below this comment. */
  /** @todo add user procedure to get transferred AP Exams below this comment. */

  getAPExamInfo: publicProcedure.query(async () => {
    return getAPExams();
  }),
  /** @todo add user procedure to get transferred GE credits below this comment. */
  /** @todo add user procedure to get transferred untransferred credits below this comment. */
});

export default transferCreditsRouter;
