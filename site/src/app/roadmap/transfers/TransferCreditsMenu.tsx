'use client';
import { FC, useEffect, useRef } from 'react';
import './TransferCreditsMenu.scss';
import { CSSTransition } from 'react-transition-group';
import { useIsMobile } from '../../../helpers/util';
import UIOverlay from '../../../component/UIOverlay/UIOverlay';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowTransfersMenu, clearUnreadTransfers } from '../../../store/slices/transferCreditsSlice';
import CoursesSection from './CoursesSection';
import APExamsSection from './APExamsSection';
import GESection from './GESection';
import UncategorizedCreditsSection from './UncategorizedCreditsSection';
import { useLoadTransferredCredits } from '../../../hooks/transferCredits';

export const ToggleTransfersButton: FC = () => {
  const isMobile = useIsMobile();
  const show = useAppSelector((state) => state.transferCredits.showTransfersMenu);
  const dispatch = useAppDispatch();

  const toggleMenu = () => {
    if (show) {
      // After closing the menu, clear all the unread markers
      dispatch(clearUnreadTransfers());
    }
    dispatch(setShowTransfersMenu(!show));
  };

  return (
    <button className={`toggle-transfers-button ${isMobile ? 'mobile' : ''}`} onClick={toggleMenu}>
      Done Editing Credits
    </button>
  );
};

export const TransferMenuContent = () => {
  return (
    <div className="transfers-menu-inner">
      <h3>Transfer Credits</h3>

      <CoursesSection />
      <APExamsSection />
      <GESection />
      <UncategorizedCreditsSection />
    </div>
  );
};

const TransferCreditsMenu: FC = () => {
  const isMobile = useIsMobile();
  const show = useAppSelector((state) => state.transferCredits.showTransfersMenu);
  useLoadTransferredCredits();

  const dispatch = useAppDispatch();

  const overlayRef = useRef<HTMLDivElement>(null);
  const sidebarRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!isMobile) return;
    overlayRef.current?.classList.toggle('enter-done', show);
  }, [isMobile, show]);

  useEffect(() => {
    if (!isMobile) dispatch(setShowTransfersMenu(false));
  }, [dispatch, isMobile]);

  const closeMenu = () => dispatch(setShowTransfersMenu(false));

  return (
    <>
      {isMobile && <UIOverlay onClick={closeMenu} zIndex={399} ref={overlayRef} />}
      <CSSTransition in={show} timeout={500} unmountOnExit nodeRef={sidebarRef}>
        <div className={`side-panel transfers-menu ${isMobile ? 'mobile' : ''}`} ref={sidebarRef}>
          <TransferMenuContent />
          <ToggleTransfersButton />
        </div>
      </CSSTransition>
    </>
  );
};

export default TransferCreditsMenu;
