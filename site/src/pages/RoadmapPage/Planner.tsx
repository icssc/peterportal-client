import { FC } from 'react';
import './Planner.scss';
import PlannerLoader from './planner/PlannerLoader';
import Header from './Header';
import Year from './Year';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';
import { useAppSelector } from '../../store/hooks';
import { selectYearPlans } from '../../store/slices/roadmapSlice';
import { useIsMobile } from '../../helpers/util';

const Planner: FC = () => {
  const currentPlanData = useAppSelector(selectYearPlans);
  const roadmapLoading = useAppSelector((state) => state.roadmap.roadmapLoading);
  const isMobile = useIsMobile();

  const maxQuarterCount = Math.max(...currentPlanData.map((y) => y.quarters.length));

  return (
    <div className={`main-wrapper ${isMobile ? 'mobile' : ''}`}>
      <div className="planner">
        <PlannerLoader />
        <Header currentPlanData={currentPlanData} />
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
    </div>
  );
};

export default Planner;
