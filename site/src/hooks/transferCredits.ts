import { useEffect, useMemo } from 'react';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import trpc from '../trpc';
import {
  setAllTransferredGEs,
  setAPExams,
  setTransferredCourses,
  setUncategorizedCourses,
  setUserAPExams,
  setDataLoadState,
  TransferredCourse,
  UserAPExam,
} from '../store/slices/transferCreditsSlice';
import { components } from '@peterportal/types/src/generated/anteater-api-types';
import {
  loadTransferredAPs,
  loadTransferredCourses,
  loadTransferredGEs,
  loadTransferredOther,
  LocalTransferSaveKey,
  saveLocalTransfers,
} from '../helpers/transferCredits';
import { useIsLoggedIn } from './isLoggedIn';
import { UncategorizedCourseEntry } from '../pages/RoadmapPage/transfers/UncategorizedCreditsSection';
import { TransferredGE } from '@peterportal/types';

/** A temporary function that returns the rewarded courses for an AP but always choosing the first choice in any given OR */
type CourseTreeItem = components['schemas']['coursesGrantedTree'] | string;
function naiveCountedCourses(courses: CourseTreeItem): string[] {
  if (typeof courses === 'string') return [courses];
  if ('AND' in courses) return (courses.AND as CourseTreeItem[]).flatMap(naiveCountedCourses);
  return naiveCountedCourses(courses.OR[0]);
}

export interface TransferredCourseWithType extends TransferredCourse {
  transferType?: 'AP' | 'Course';
}

export function useTransferredCredits() {
  const apExamInfo = useAppSelector((state) => state.transferCredits.apExamInfo);
  const transferredCourses = useAppSelector((state) => state.transferCredits.transferredCourses);
  const apTransfers = useAppSelector((state) => state.transferCredits.userAPExams);
  const ge = useAppSelector((state) => state.transferCredits.transferredGEs);
  const other = useAppSelector((state) => state.transferCredits.uncategorizedCourses);

  const courses = useMemo(() => {
    // Recomputes this value only when APs or Transferred Courses update
    const rewardedCourses: TransferredCourseWithType[] = apTransfers.flatMap((transfer) => {
      const info = apExamInfo.find((ap) => ap.fullName === transfer.examName);
      if (!info) return [];
      const reward = info.rewards.find((r) => r.acceptableScores.includes(transfer.score));
      if (!reward) return [];

      const courseNames = naiveCountedCourses(reward.coursesGranted);
      /** @todo debug print statement – remove once a user can choose which 'OR' reward they want */
      // console.log(info.catalogueName ?? info.fullName, '=>', courseNames)

      return courseNames.map((courseName) => ({ courseName, units: 0, transferType: 'AP' }));
    });

    const clearedCourses: TransferredCourseWithType[] = transferredCourses
      .map((course) => ({ ...course, transferType: 'Course' }) as TransferredCourseWithType)
      .concat(rewardedCourses);
    return [...new Set(clearedCourses)];
  }, [apExamInfo, apTransfers, transferredCourses]);

  // Returns memoized result for efficiency and so we can use the entire return value
  // as a dependency array item. Without memoization, that would not be possible, as
  // a new object would be created every time.
  const memoized = useMemo(
    () => ({ courses, apInfo: apExamInfo, ap: apTransfers, ge, other }),
    [apExamInfo, courses, apTransfers, ge, other],
  );
  return memoized;
}

export function useLoadTransferredCredits() {
  const isLoggedIn = useIsLoggedIn();
  const dataLoadingState = useAppSelector((state) => state.transferCredits.dataLoadState);
  const userDataLoaded = dataLoadingState === 'done';

  // Use raw data rather than useClearedCourses() because we want just the
  // user's input (not implicit courses) to be saved
  const transferredCourses = useAppSelector((state) => state.transferCredits.transferredCourses);
  const transferredAPs = useAppSelector((state) => state.transferCredits.userAPExams);
  const transferredGEs = useAppSelector((state) => state.transferCredits.transferredGEs);
  const transferredOther = useAppSelector((state) => state.transferCredits.uncategorizedCourses);

  const dispatch = useAppDispatch();

  useEffect(() => {
    if (dataLoadingState !== 'loading') return;

    const loadAPInfo = trpc.transferCredits.getAPExamInfo.query().then((exams) => {
      const parsedExams = exams.map((exam) => ({
        ...exam,
        catalogueName: (exam.catalogueName as string) ?? null,
      }));
      dispatch(setAPExams(parsedExams));
    });

    const loadCourses = loadTransferredCourses(isLoggedIn).then((res) => dispatch(setTransferredCourses(res)));
    const loadAPs = loadTransferredAPs(isLoggedIn).then((res) => dispatch(setUserAPExams(res)));
    const loadGEs = loadTransferredGEs(isLoggedIn).then((res) => dispatch(setAllTransferredGEs(res)));
    const loadOther = loadTransferredOther(isLoggedIn).then((res) => dispatch(setUncategorizedCourses(res)));

    // Load in parallel
    Promise.all([loadAPInfo, loadCourses, loadAPs, loadGEs, loadOther]).then(() => dispatch(setDataLoadState('done')));
  }, [dispatch, isLoggedIn, dataLoadingState]);

  const { Course: CourseKey, AP: APKey, GE: GEKey, Uncategorized: OtherKey } = LocalTransferSaveKey;

  // Save to localStorage whenever any transferred credit data changes
  useEffect(() => {
    if (isLoggedIn || !userDataLoaded) return;
    saveLocalTransfers<TransferredCourse>(CourseKey, transferredCourses);
  }, [isLoggedIn, CourseKey, transferredCourses, userDataLoaded]);

  useEffect(() => {
    if (isLoggedIn || !userDataLoaded) return;
    saveLocalTransfers<UserAPExam>(APKey, transferredAPs);
  }, [isLoggedIn, APKey, transferredAPs, userDataLoaded]);

  useEffect(() => {
    if (isLoggedIn || !userDataLoaded) return;
    saveLocalTransfers<TransferredGE>(GEKey, transferredGEs);
  }, [isLoggedIn, GEKey, transferredGEs, userDataLoaded]);

  useEffect(() => {
    if (isLoggedIn || !userDataLoaded) return;
    saveLocalTransfers<UncategorizedCourseEntry>(OtherKey, transferredOther);
  }, [isLoggedIn, OtherKey, transferredOther, userDataLoaded]);
}
