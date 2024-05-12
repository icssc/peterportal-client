import { useEffect, useState } from 'react';
import './ChangelogModal.scss';
import Modal from 'react-bootstrap/Modal';
import changelogImage from '../../asset/multiplanner.png';

const DESCRIPTION = 'You are now able to add, edit, and delete multiple 4-year plans within your roadmap!';
const LAST_UPDATED = '05/06/2024';

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
    <Modal className="changelog-modal" show={showModal} centered onHide={closeModal}>
      <Modal.Header closeButton>
        <h2>
          What's New &ndash; {new Date(LAST_UPDATED).toLocaleString('default', { month: 'long', year: 'numeric' })}
        </h2>
      </Modal.Header>

      <p className="modal-body">{DESCRIPTION}</p>
      <img className="modal-img" src={changelogImage} alt="Screenshot or gif of new changes" />
    </Modal>
  );
};

export default ChangelogModal;
