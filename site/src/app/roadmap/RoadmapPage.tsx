'use client';
import { FC, useEffect, useState, useMemo } from 'react';
import './RoadmapPage.scss';
import Planner from './planner/Planner';
import MobileCourseCatalog from './catalog/MobileCourseCatalog';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import AddCoursePopup from './planner/AddCoursePopup';
import { useIsMobile } from '../../helpers/util';
import CoursePreview from '../../component/ResultPreview/CoursePreview';
import DesktopRoadmapSidebar from './sidebar/DesktopRoadmapSidebar';
import { MobileCreditsMenu } from './transfers/MobileCreditsMenu';
import { hideMobileCatalog, setShowToast } from '../../store/slices/roadmapSlice';
import Toast from '../../helpers/toast';
import ProfessorPreview from '../../component/ResultPreview/ProfessorPreview';
import MobileSearchMenu from '../../component/MobileSearchMenu/MobileSearchMenu';
import MobilePopup from './MobilePopup';
import { Fade, useTheme } from '@mui/material';
import { usePathname, useRouter, useSearchParams } from 'next/navigation';
import { usePreviewDepth } from '../../hooks/usePreviewDepth';
import MobileNavbar from './MobileNavbar';
import { setSelectedMobileTab, setShowMobileFullscreenSearch, showMobileCatalog } from '../../store/slices/roadmapSlice';
import { setShowMobileCreditsMenu } from '../../store/slices/transferCreditsSlice';

const RoadmapPage: FC = () => {
  const isMobile = useIsMobile();
  const dispatch = useAppDispatch();

  const toastMsg = useAppSelector((state) => state.roadmap.toastMsg);
  const toastSeverity = useAppSelector((state) => state.roadmap.toastSeverity);
  const showToast = useAppSelector((state) => state.roadmap.showToast);
  const showFullscreenSearch = useAppSelector((state) => state.roadmap.showMobileFullscreenSearch);

  const theme = useTheme();
  const transitionTime = theme.transitions.duration.shortest;
  const router = useRouter();
  const searchParams = useSearchParams();
  const pathname = usePathname();

  const courseParam = searchParams.get('course');
  const instructorParam = searchParams.get('instructor');
  const selectedMobileIndex = useAppSelector((state) => state.roadmap.selectedMobileTab);

  const currentPreview = useMemo(() => {
    return courseParam
      ? ({ type: 'course', id: courseParam } as const)
      : instructorParam
        ? ({ type: 'instructor', id: instructorParam } as const)
        : null;
  }, [courseParam, instructorParam]);
  const [showPreview, setShowPreview] = useState(false);
  useEffect(() => {
    if (currentPreview) setShowPreview(true);
    else setShowPreview(false);
  }, [currentPreview]);

  const previewDepth = usePreviewDepth();
  const handleCloseToast = () => dispatch(setShowToast(false));
  const fullscreenActive = isMobile && showFullscreenSearch;

  const handleClosePreview = () => {
    setShowPreview(false);
    setTimeout(() => {
      router.push(pathname);
    }, transitionTime);
  };

  const handleBackPreview = () => {
    router.back();
  };

  const resultPreview = (
    <div>
      {currentPreview &&
        (currentPreview.type === 'course' ? (
          <CoursePreview
            courseId={currentPreview.id}
            onClose={handleClosePreview}
            onBack={handleBackPreview}
            showBack={previewDepth > 1}
          />
        ) : (
          <ProfessorPreview
            netid={currentPreview.id}
            onClose={handleClosePreview}
            onBack={handleBackPreview}
            showBack={previewDepth > 1}
          />
        ))}
    </div>
  );

  const handleMobileTabChange = (newValue: number) => {
    dispatch(setSelectedMobileTab(newValue));

    switch (newValue) {
      case 1:
        dispatch(setShowMobileCreditsMenu(true));
        dispatch(hideMobileCatalog());
        dispatch(setShowMobileFullscreenSearch(false));
        break;
      case 2:
        dispatch(setShowMobileCreditsMenu(false));
        dispatch(showMobileCatalog({ year: 0, quarter: 0 }));
        dispatch(setShowMobileFullscreenSearch(false));
        break;
      case 3:
        dispatch(setShowMobileCreditsMenu(false));
        dispatch(hideMobileCatalog());
        dispatch(setShowMobileFullscreenSearch(true));
        break;
      default:
        dispatch(setShowMobileCreditsMenu(false));
        dispatch(hideMobileCatalog());
        dispatch(setShowMobileFullscreenSearch(false));
    }
  };

  return (
    <div className="roadmap-page">
      {!isMobile ? (
        <>
          <DesktopRoadmapSidebar />

        <Toast text={toastMsg} severity={toastSeverity} showToast={showToast} onClose={handleCloseToast} />

        {/* Main Planner View or Fullscreen Mobile Search */}
        <div className="main-wrapper" id="mobileScrollContainer">
          <Planner />
            <Fade in={showPreview} timeout={{ enter: 0, exit: transitionTime }}>
              {resultPreview}
            </Fade>
        </div>
      </>
      ) : (
        <div>
          <Toast text={toastMsg} severity={toastSeverity} showToast={showToast} onClose={handleCloseToast} />
          <AddCoursePopup />
          <MobileCourseCatalog />
          <MobileCreditsMenu />

          {fullscreenActive ? <MobileSearchMenu /> : <Planner />}

          <MobilePopup show={showPreview} onClose={handleClosePreview}>
            {resultPreview}
          </MobilePopup>

          <div className={`main-wrapper mobile`} id="mobileScrollContainer">
            {selectedMobileIndex === 0 && <Planner />}
          </div>

        </div>
      )}
      {isMobile && <MobileNavbar selectedMobileIndex={selectedMobileIndex} onTabChange={handleMobileTabChange}/>}
    </div>
  );
};

export default RoadmapPage;
