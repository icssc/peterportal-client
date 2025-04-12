import { FC, useEffect, useRef } from 'react';
import './TransferCreditsMenu.scss';
import { ArrowLeftRight } from 'react-bootstrap-icons';
import { CSSTransition } from 'react-transition-group';
import { useIsMobile } from '../../../helpers/util';
import UIOverlay from '../../../component/UIOverlay/UIOverlay';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowTransfersMenu } from '../../../store/slices/transferCreditsSlice';
import CoursesSection from './CoursesSection';
import APExamsSection from './APExamsSection';
import GESection from './GESection';
import UncategorizedCreditsSection from './UncategorizedCreditsSection';

export const ToggleTransfersButton: FC = () => {
  const isMobile = useIsMobile();
  const show = useAppSelector((state) => state.transferCredits.showTransfersMenu);
  const dispatch = useAppDispatch();

  const toggleMenu = () => dispatch(setShowTransfersMenu(!show));

  return (
    <button className={`toggle-transfers-button ${isMobile ? 'mobile' : ''}`} onClick={toggleMenu}>
      {show ? (
        <>Done Editing Credits</>
      ) : (
        <>
          Transfer Credits <ArrowLeftRight />
        </>
      )}
    </button>
  );
};

const TransferCreditsMenu: FC = () => {
  const isMobile = useIsMobile();
  const show = useAppSelector((state) => state.transferCredits.showTransfersMenu);

  const dispatch = useAppDispatch();

  const overlayRef = useRef<HTMLDivElement>(null);
  const sidebarRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!isMobile) return;
    sidebarRef.current?.classList.toggle('enter-done', show);
    overlayRef.current?.classList.toggle('enter-done', show);
  }, [isMobile, show]);

  useEffect(() => {
    if (!isMobile) dispatch(setShowTransfersMenu(false));
  }, [dispatch, isMobile]);

  const closeMenu = () => dispatch(setShowTransfersMenu(false));

  return (
    <>
      {isMobile && <UIOverlay onClick={closeMenu} zIndex={399} passedRef={overlayRef} />}
      <CSSTransition in={show} timeout={500} unmountOnExit>
        <div className={`side-panel search-sidebar transfers-menu ${isMobile ? 'mobile' : ''}`} ref={sidebarRef}>
          <h3>Transfer Credits</h3>

          <CoursesSection />
          <APExamsSection />
          <GESection />
          <UncategorizedCreditsSection />
        </div>
      </CSSTransition>
      <ToggleTransfersButton />
    </>
  );
};

export default TransferCreditsMenu;
