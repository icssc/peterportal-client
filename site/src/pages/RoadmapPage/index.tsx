import { FC } from 'react';
import './index.scss';
import AddCoursePopup from './AddCoursePopup';
import Planner from './Planner';
import SearchSidebar from './SearchSidebar';
import TransferCreditsMenu from './transfers/TransferCreditsMenu';
import { CSSTransition } from 'react-transition-group';
import { useAppSelector } from '../../store/hooks';
import { useIsMobile } from '../../helpers/util';

const RoadmapPage: FC = () => {
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const isMobile = useIsMobile();

  return (
    <div className="roadmap-page">
      <AddCoursePopup />
      <Planner />
      <CSSTransition in={!isMobile || showSearch} timeout={500} unmountOnExit>
        <SearchSidebar />
      </CSSTransition>
      {isMobile && <TransferCreditsMenu />}
    </div>
  );
};

export default RoadmapPage;
