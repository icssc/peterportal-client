'use client';
import { FC, useEffect, useState } from 'react';
import './RoadmapPage.scss';
import Planner from './planner/Planner';
import MobileCourseCatalog from './catalog/MobileCourseCatalog';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import AddCoursePopup from './planner/AddCoursePopup';
import { useIsMobile } from '../../helpers/util';
import CoursePreview from '../../component/ResultPreview/CoursePreview';
import DesktopRoadmapSidebar from './sidebar/DesktopRoadmapSidebar';
import { MobileCreditsMenu } from './transfers/MobileCreditsMenu';
import { setShowToast } from '../../store/slices/roadmapSlice';
import Toast from '../../helpers/toast';
import ProfessorPreview from '../../component/ResultPreview/ProfessorPreview';
import MobileSearchMenu from '../../component/MobileSearchMenu/MobileSearchMenu';
import MobilePopup from './MobilePopup';
import { Fade, useTheme } from '@mui/material';
import { setPreviewedCourse, setPreviewedProfessor } from '../../store/slices/coursePreviewSlice';

const RoadmapPage: FC = () => {
  const isMobile = useIsMobile();

  const previewCourseId = useAppSelector((state) => state.coursePreview.courseId);
  const previewProfId = useAppSelector((state) => state.coursePreview.professorId);

  const dispatch = useAppDispatch();

  const toastMsg = useAppSelector((state) => state.roadmap.toastMsg);
  const toastSeverity = useAppSelector((state) => state.roadmap.toastSeverity);
  const showToast = useAppSelector((state) => state.roadmap.showToast);
  const showFullscreenSearch = useAppSelector((state) => state.roadmap.showMobileFullscreenSearch);

  const [showPreview, setShowPreview] = useState(false);
  const theme = useTheme();
  const transitionTime = theme.transitions.duration.shortest;

  const handleCloseToast = () => {
    dispatch(setShowToast(false));
  };

  const fullscreenActive = isMobile && showFullscreenSearch;

  const handleClosePreview = () => {
    setShowPreview(false);
    setTimeout(() => {
      dispatch(setPreviewedCourse(''));
      dispatch(setPreviewedProfessor(''));
    }, transitionTime);
  };

  useEffect(() => {
    if (previewCourseId || previewProfId) setShowPreview(true);
  }, [previewCourseId, previewProfId]);

  const resultPreview = (
    <div>
      {previewCourseId && <CoursePreview courseId={previewCourseId} onClose={handleClosePreview} />}
      {previewProfId && <ProfessorPreview netid={previewProfId} onClose={handleClosePreview} />}
    </div>
  );

  return (
    <div className="roadmap-page">
      {!isMobile && <DesktopRoadmapSidebar />}

      {/* Mobile Popup Menus */}
      <Toast text={toastMsg} severity={toastSeverity} showToast={showToast} onClose={handleCloseToast} />
      <AddCoursePopup />
      <MobileCourseCatalog />
      <MobileCreditsMenu />

      {/* Main Planner View or Fullscreen Mobile Search */}
      <div className={`main-wrapper ${isMobile ? 'mobile' : ''}`} id="mobileScrollContainer">
        {fullscreenActive ? <MobileSearchMenu /> : <Planner />}
        {isMobile ? (
          <MobilePopup show={showPreview} onClose={handleClosePreview}>
            {resultPreview}
          </MobilePopup>
        ) : (
          <Fade in={showPreview} timeout={{ enter: 0, exit: transitionTime }}>
            {resultPreview}
          </Fade>
        )}
      </div>
    </div>
  );
};

export default RoadmapPage;
