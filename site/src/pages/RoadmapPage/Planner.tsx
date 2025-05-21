import { FC } from 'react';
import './Planner.scss';
import Header from './Header';
import AddYearPopup from './AddYearPopup';
import Year from './Year';
import { useAppSelector } from '../../store/hooks';
import { RoadmapPlan, selectAllPlans, selectYearPlans } from '../../store/slices/roadmapSlice';
import ImportTranscriptPopup from './ImportTranscriptPopup';
import ImportZot4PlanPopup from './ImportZot4PlanPopup';
import { getTotalUnitsFromTransfers } from '../../helpers/transferCredits';
import { useTransferredCredits } from '../../hooks/transferCredits';
import PlannerLoader from './planner/PlannerLoader';
import { collapseAllPlanners, saveRoadmap } from '../../helpers/planner';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';

import { Spinner } from 'react-bootstrap';

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
      {roadmapLoading ? (
        <div className="loading-spinner">
          <Spinner animation="border" />
        </div>
      ) : (
        <>
          <Header
            courseCount={courseCount}
            unitCount={unitCount}
            saveRoadmap={handleSave}
            missingPrerequisites={new Set()}
          />
          <section className="years" data-max-quarter-count={maxQuarterCount}>
            {currentPlanData.map((year, yearIndex) => {
              return <Year key={yearIndex} yearIndex={yearIndex} data={year} />;
            })}
          </section>
          <div className="action-row">
            <AddYearPopup
              placeholderName={'Year ' + (currentPlanData.length + 1)}
              placeholderYear={
                currentPlanData.length === 0
                  ? new Date().getFullYear()
                  : currentPlanData[currentPlanData.length - 1].startYear + 1
              }
            />
            <ImportTranscriptPopup />
            <ImportZot4PlanPopup saveRoadmap={handleSave} />
          </div>
        </>
      )}
    </div>
  );
};
export default Planner;
