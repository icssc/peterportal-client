'use client';
import { FC, useEffect } from 'react';
import './TransferCreditsMenu.scss';
import { useIsMobile } from '../../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { setShowTransfersMenu, clearUnreadTransfers } from '../../../store/slices/transferCreditsSlice';
import CoursesSection from './CoursesSection';
import APExamsSection from './APExamsSection';
import GESection from './GESection';
import UncategorizedCreditsSection from './UncategorizedCreditsSection';
import { useLoadTransferredCredits } from '../../../hooks/transferCredits';
import MobilePopup from '../MobilePopup';

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
    <div>
      <h3>Add Course Credits</h3>

      <CoursesSection />
      <APExamsSection />
      <GESection />
      <UncategorizedCreditsSection />
    </div>
  );
};

export const MobileCreditsMenu: FC = () => {
  const isMobile = useIsMobile();
  const show = useAppSelector((state) => state.transferCredits.showTransfersMenu);
  useLoadTransferredCredits();

  const dispatch = useAppDispatch();

  /** @todo move out of global state since this will no longer be conditionally rendered */
  useEffect(() => {
    if (!isMobile) dispatch(setShowTransfersMenu(false));
  }, [dispatch, isMobile]);

  const closeMenu = () => dispatch(setShowTransfersMenu(false));

  return (
    <MobilePopup show={show} className="transfers-menu" onClose={closeMenu}>
      <TransferMenuContent />
      <ToggleTransfersButton />
    </MobilePopup>
  );
};
