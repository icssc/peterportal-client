'use client';
import { FC } from 'react';
import './Planner.scss';
import PlannerLoader from './PlannerLoader';
import Header from '../toolbar/Header';
import Year from './Year';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import { useAppSelector } from '../../../store/hooks';
import { selectYearPlans } from '../../../store/slices/roadmapSlice';
import { getTotalUnitsFromTransfers } from '../../../helpers/transferCredits';
import { useTransferredCredits } from '../../../hooks/transferCredits';

const Planner: FC = () => {
  const currentPlanData = useAppSelector(selectYearPlans);
  const roadmapLoading = useAppSelector((state) => state.roadmap.roadmapLoading);
  const transferred = useTransferredCredits();

  const calculatePlannerOverviewStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    // sum up all courses
    const courses = currentPlanData.flatMap((year) => year.quarters).flatMap((q) => q.courses);
    courses.forEach((course) => {
      unitCount += course.minUnits;
      courseCount++;
    });

    // add in transfer courses
    courseCount += transferred.courses.length;
    unitCount += getTotalUnitsFromTransfers(transferred.courses, transferred.ap, transferred.ge, transferred.other);
    return { unitCount, courseCount };
  };

  const { unitCount, courseCount } = calculatePlannerOverviewStats();

  const quarterCounts = currentPlanData.map((years) => years.quarters.length);
  const maxQuarterCount = Math.max(...quarterCounts);

  return (
    <div className="planner">
      <PlannerLoader />
      <Header courseCount={courseCount} unitCount={unitCount} missingPrerequisites={new Set()} />
      {roadmapLoading ? (
        <LoadingSpinner />
      ) : (
        <section className="years" data-max-quarter-count={maxQuarterCount}>
          {currentPlanData.map((year, yearIndex) => {
            return <Year key={yearIndex} yearIndex={yearIndex} data={year} />;
          })}
        </section>
      )}
    </div>
  );
};

export default Planner;
