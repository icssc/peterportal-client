'use client';
import { useEffect } from 'react';
import './ChangelogModal.scss';
import changelogImage from '../../asset/rm-dedicated-search-pgs.png';
import Image from 'next/image';
import { Button, Dialog, DialogActions, DialogContent, DialogContentText, DialogTitle } from '@mui/material';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setChangelogOpen } from '../../store/slices/uiSlice';

const DESCRIPTION = 'Course and instructor search are now integrated into the main roadmap search menus!';
const LAST_UPDATED = '12/04/2025';

const ChangelogModal = () => {
  const dispatch = useAppDispatch();
  const showModal = useAppSelector((state) => state.ui.changelogOpen);

  useEffect(() => {
    // display the changelog modal if it is the user's first time seeing it (tracked in local storage)
    const lastSeen = localStorage.getItem('changelogSeen');

    if (lastSeen !== LAST_UPDATED) {
      // mark as seen so it is not displayed after seeing it once
      localStorage.setItem('changelogSeen', LAST_UPDATED);

      dispatch(setChangelogOpen(true));
    }
  }, [dispatch]);

  const closeModal = () => {
    dispatch(setChangelogOpen(false));
  };

  return (
    <Dialog open={showModal} onClose={closeModal} className="changelog-modal">
      <DialogTitle>
        What's New &ndash; {new Date(LAST_UPDATED).toLocaleString('default', { month: 'long', year: 'numeric' })}
      </DialogTitle>
      <DialogContent>
        <DialogContentText>{DESCRIPTION}</DialogContentText>
        <Image
          className="modal-img"
          src={changelogImage.src}
          width={changelogImage.width}
          height={changelogImage.height}
          alt="Screenshot or gif of new changes"
        />
      </DialogContent>
      <DialogActions>
        <Button variant="text" color="inherit" onClick={closeModal}>
          Close
        </Button>
      </DialogActions>
    </Dialog>
  );
};

export default ChangelogModal;
