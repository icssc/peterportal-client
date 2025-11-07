'use client';
import { FC } from 'react';
import './RoadmapPage.scss';
import Planner from './planner/Planner';
import MobileCourseCatalog from './catalog/MobileCourseCatalog';
import { useAppSelector } from '../../store/hooks';
import AddCoursePopup from './planner/AddCoursePopup';
import { useIsMobile } from '../../helpers/util';
import { MobileCreditsMenu } from './transfers/TransferCreditsMenu';
import CoursePreview from '../../component/CoursePreview/CoursePreview';
import DesktopRoadmapSidebar from './sidebar/DesktopRoadmapSidebar';

const RoadmapPage: FC = () => {
  const isMobile = useIsMobile();

  const previewCourseId = useAppSelector((state) => state.coursePreview.courseId);

  return (
    <div className="roadmap-page">
      {!isMobile && <DesktopRoadmapSidebar />}

      {/* Mobile Popup Menus */}
      <AddCoursePopup />
      <MobileCourseCatalog />
      <MobileCreditsMenu />

      {/* Main Planner View */}
      <div className={`main-wrapper ${isMobile ? 'mobile' : ''}`}>
        <Planner />
        {previewCourseId && <CoursePreview courseId={previewCourseId} />}
      </div>
    </div>
  );
};

export default RoadmapPage;
