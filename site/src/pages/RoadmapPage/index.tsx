import { FC } from 'react';
import { CSSTransition } from 'react-transition-group';
import './index.scss';

import Planner from './Planner';
import SearchSidebar from './SearchSidebar';
import AddCoursePopup from './AddCoursePopup';
import TransferCreditsMenu from './transfers/TransferCreditsMenu';

import { useAppSelector } from '../../store/hooks';
import { useIsMobile } from '../../helpers/util';

const RoadmapPage: FC = () => {
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();

  return (
    <div className="roadmap-page">
      <AddCoursePopup />
      <div className={`main-wrapper ${isMobile ? 'mobile' : ''}`}>
        <Planner />
      </div>
      <CSSTransition in={!isMobile || showSearch} timeout={500} unmountOnExit>
        <SearchSidebar />
      </CSSTransition>
      {isMobile && <TransferCreditsMenu />}
    </div>
  );
};

export default RoadmapPage;
