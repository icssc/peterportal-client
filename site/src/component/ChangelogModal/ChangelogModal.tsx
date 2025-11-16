'use client';
import { useEffect, useState } from 'react';
import './ChangelogModal.scss';
import changelogImage from '../../asset/course-preview.png';
import Image from 'next/image';
import { Dialog, DialogContent, DialogContentText, DialogTitle } from '@mui/material';

const DESCRIPTION = 'You can now click on courses in the sidebar to preview them without leaving your roadmap!';
const LAST_UPDATED = '11/03/2025';

const ChangelogModal = () => {
  const [showModal, setShowModal] = useState(false);

  useEffect(() => {
    // display the changelog modal if it is the user's first time seeing it (tracked in local storage)
    const lastSeen = localStorage.getItem('changelogSeen');

    if (lastSeen !== LAST_UPDATED) {
      setShowModal(true);

      // mark as seen so it is not displayed after seeing it once
      localStorage.setItem('changelogSeen', LAST_UPDATED);
    }
  }, []);

  const closeModal = () => {
    setShowModal(false);
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
    </Dialog>
  );
};

export default ChangelogModal;
