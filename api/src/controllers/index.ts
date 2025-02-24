import { router } from '../helpers/trpc';
import coursesRouter from './courses';
import professorsRouter from './professors';
import programsRouter from './programs';
import reportsRouter from './reports';
import reviewsRouter from './reviews';
import roadmapsRouter from './roadmap';
import { savedCoursesRouter } from './savedCourses';
import scheduleRouter from './schedule';
import usersRouter from './users';
import searchRouter from './search';
import zot4PlanImportRouter from './zot4planimport';
import { externalAppRouter } from './external';

export const appRouter = router({
  external: externalAppRouter,
  courses: coursesRouter,
  professors: professorsRouter,
  programs: programsRouter,
  roadmaps: roadmapsRouter,
  reports: reportsRouter,
  reviews: reviewsRouter,
  savedCourses: savedCoursesRouter,
  search: searchRouter,
  schedule: scheduleRouter,
  users: usersRouter,
  zot4PlanImportRouter: zot4PlanImportRouter,
});

// Export only the type of a router!
// This prevents us from importing server code on the client.
export type AppRouter = typeof appRouter;
