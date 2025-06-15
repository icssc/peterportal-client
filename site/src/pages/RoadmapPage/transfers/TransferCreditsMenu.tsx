import { FC, useEffect, useCallback } from 'react';
import './TransferCreditsMenu.scss';
import { CSSTransition } from 'react-transition-group';

import UIOverlay from '../../../component/UIOverlay/UIOverlay';
import CoursesSection from './CoursesSection';
import APExamsSection from './APExamsSection';
import GESection from './GESection';
import UncategorizedCreditsSection from './UncategorizedCreditsSection';

import { setShowTransfersMenu } from '../../../store/slices/transferCreditsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { useLoadTransferredCredits, useToggleTransfers } from '../../../hooks/transferCredits';
import { useToggleRef } from '../../../hooks/planner';
import { useIsMobile } from '../../../helpers/util';

const TransferCreditsMenu: FC = () => {
  const show = useAppSelector((state) => state.transferCredits.showTransfersMenu);

  const dispatch = useAppDispatch();
  const isMobile = useIsMobile();
  useLoadTransferredCredits();
  const setToggleTransfers = useToggleTransfers();
  const { overlayRef, sidebarRef } = useToggleRef(isMobile, show);

  const closeMenu = useCallback(() => dispatch(setShowTransfersMenu(false)), [dispatch]);
  const toggleTransfers = () => setToggleTransfers(show);

  useEffect(() => {
    if (isMobile) return;
    closeMenu();
  }, [isMobile, closeMenu]);

  const ToggleTransfersButton = () => (
    <button className={`toggle-transfers-button ${isMobile ? 'mobile' : ''}`} onClick={toggleTransfers}>
      Done Editing Credits
    </button>
  );

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
          <ToggleTransfersButton />
        </div>
      </CSSTransition>
    </>
  );
};

export default TransferCreditsMenu;
