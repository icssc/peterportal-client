import { FC } from 'react';
import './Planner.scss';
import PlannerLoader from './planner/PlannerLoader';
import Header from './Header';
import Year from './Year';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';
import { useAppSelector } from '../../store/hooks';
import { RoadmapPlan, selectAllPlans, selectYearPlans } from '../../store/slices/roadmapSlice';
import { getTotalUnitsFromTransfers } from '../../helpers/transferCredits';
import { collapseAllPlanners, saveRoadmap } from '../../helpers/planner';
import { useTransferredCredits } from '../../hooks/transferCredits';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';

const Planner: FC = () => {
  const allPlanData = useAppSelector(selectAllPlans);
  const currentPlanData = useAppSelector(selectYearPlans);
  const roadmapLoading = useAppSelector((state) => state.roadmap.roadmapLoading);
  const transferred = useTransferredCredits();
  const isLoggedIn = useIsLoggedIn();

  const handleSave = async (plans?: RoadmapPlan[]) => {
    const collapsed = collapseAllPlanners(plans?.length ? plans : allPlanData);
    saveRoadmap(isLoggedIn, collapsed, true);
  };

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
      <Header
        courseCount={courseCount}
        unitCount={unitCount}
        saveRoadmap={handleSave}
        missingPrerequisites={new Set()}
      />
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
