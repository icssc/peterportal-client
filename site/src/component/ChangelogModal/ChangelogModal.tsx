import { useEffect, useState } from 'react';
import './ChangelogModal.scss';
import Button from 'react-bootstrap/Button';
import Modal from 'react-bootstrap/Modal';
import Row from 'react-bootstrap/Row';

const DESCRIPTION = 'You can now view recently added features to the PeterPortal website, listed in this modal.';
const IMAGE_URL =
  'https://media.tenor.com/ufm_0t3ACEAAAAAM/ginger-cat-ginger-cat-eating-then-staring-at-the-camera.gif';
const LAST_UPDATED = '02/27/2024';

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
    <div onClick={closeModal}>
      <Modal
        className="changelog-modal"
        show={showModal}
        centered
        animation={false}
        onClick={(e: MouseEvent) => e.stopPropagation()}
      >
        <Modal.Header closeButton>
          <h2>What's New - {new Date().toLocaleString('default', { month: 'long', year: 'numeric' })}</h2>
        </Modal.Header>

        <p className="modal-body">{DESCRIPTION}</p>
        <img className="modal-img" src={IMAGE_URL} />
      </Modal>
    </div>
  );
};

export default ChangelogModal;
