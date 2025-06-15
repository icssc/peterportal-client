import { FC, useEffect, useRef, useCallback } from 'react';
import './TransferCreditsMenu.scss';
import { CSSTransition } from 'react-transition-group';
import { useIsMobile } from '../../../helpers/util';
import UIOverlay from '../../../component/UIOverlay/UIOverlay';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowTransfersMenu } from '../../../store/slices/transferCreditsSlice';
import CoursesSection from './CoursesSection';
import APExamsSection from './APExamsSection';
import GESection from './GESection';
import UncategorizedCreditsSection from './UncategorizedCreditsSection';
import { useLoadTransferredCredits, useToggleTransfers } from '../../../hooks/transferCredits';

const TransferCreditsMenu: FC = () => {
  const show = useAppSelector((state) => state.transferCredits.showTransfersMenu);

  const isMobile = useIsMobile();
  useLoadTransferredCredits();
  const toggleTransfers = useToggleTransfers();

  const dispatch = useAppDispatch();

  const closeMenu = useCallback(() => dispatch(setShowTransfersMenu(false)), [dispatch]);
  const toggleMenu = () => toggleTransfers(show);

  const overlayRef = useRef<HTMLDivElement>(null);
  const sidebarRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!isMobile) return;
    sidebarRef.current?.classList.toggle('enter-done', show);
    overlayRef.current?.classList.toggle('enter-done', show);
  }, [isMobile, show]);

  useEffect(() => {
    if (isMobile) return;
    closeMenu();
  }, [isMobile, closeMenu]);

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
          <button className={`toggle-transfers-button ${isMobile ? 'mobile' : ''}`} onClick={toggleMenu}>
            Done Editing Credits
          </button>
        </div>
      </CSSTransition>
    </>
  );
};

export default TransferCreditsMenu;
