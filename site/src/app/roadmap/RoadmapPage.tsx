'use client';
import { FC, useEffect, useRef, useState } from 'react';
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
import { usePathname, useRouter, useSearchParams } from 'next/navigation';

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
  const currentPreview = courseParam
    ? ({ type: 'course', id: courseParam } as const)
    : instructorParam
      ? ({ type: 'instructor', id: instructorParam } as const)
      : null;

  const [showPreview, setShowPreview] = useState(false);
  useEffect(() => {
    if (currentPreview) setShowPreview(true);
    else setShowPreview(false);
  }, [currentPreview]);

  const depthRef = useRef(0);
  const [previewDepth, setPreviewDepth] = useState(0);
  useEffect(() => {
    const original = history.pushState.bind(history);
    history.pushState = function (...args: Parameters<typeof history.pushState>) {
      const urlArg = args[2];
      if (urlArg != null) {
        const url = new URL(urlArg.toString(), window.location.href);
        const newDepth =
          url.searchParams.has('course') || url.searchParams.has('instructor') ? depthRef.current + 1 : 0;
        depthRef.current = newDepth;
        args[0] = { ...args[0], __previewDepth: newDepth };
        queueMicrotask(() => setPreviewDepth(newDepth));
      }
      return original(...args);
    };
    return () => {
      history.pushState = original;
    };
  }, []);

  useEffect(() => {
    const onPopState = (e: PopStateEvent) => {
      const newDepth = e.state?.__previewDepth ?? 0;
      depthRef.current = newDepth;
      setPreviewDepth(newDepth);
    };
    window.addEventListener('popstate', onPopState);
    return () => window.removeEventListener('popstate', onPopState);
  }, []);

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
