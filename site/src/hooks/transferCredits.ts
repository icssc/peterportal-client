import { useMemo } from 'react';
import { useAppSelector } from '../store/hooks';

export function useTransferredCredits() {
  const apExamInfo = useAppSelector((state) => state.transferCredits.apExamInfo);
  const courses = useAppSelector((state) => state.transferCredits.transferredCourses);
  const apTransfers = useAppSelector((state) => state.transferCredits.userAPExams);
  const ge = useAppSelector((state) => state.transferCredits.transferredGEs);
  const other = useAppSelector((state) => state.transferCredits.uncategorizedCourses);

  // Returns memoized result for efficiency and so we can use the entire return value
  // as a dependency array item. Without memoization, that would not be possible, as
  // a new object would be created every time.
  const memoized = useMemo(
    () => ({ courses, apInfo: apExamInfo, ap: apTransfers, ge, other }),
    [apExamInfo, courses, apTransfers, ge, other],
  );
  return memoized;
}
