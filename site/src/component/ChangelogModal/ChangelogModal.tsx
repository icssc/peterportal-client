import { useEffect, useState } from 'react';
import './ChangelogModal.scss';
import Button from 'react-bootstrap/Button';
import Modal from 'react-bootstrap/Modal';
import Row from 'react-bootstrap/Row';

const DESCRIPTION = 'You can now view recently added features to the PeterPortal website, listed in this modal.';
const IMAGE_URL =
  'https://media.tenor.com/ufm_0t3ACEAAAAAM/ginger-cat-ginger-cat-eating-then-staring-at-the-camera.gif';

const ChangelogModal = () => {
  const [showModal, setShowModal] = useState(false);

  const currentMonth = new Date().toLocaleString('default', { month: 'long', year: 'numeric' });

  useEffect(() => {
    // display the changelog modal if it is the user's first time seeing it (tracked in local storage)
    const lastSeen = localStorage.getItem('changelogSeen');

    if (lastSeen !== currentMonth) {
      setShowModal(true);

      // mark as seen so it is not displayed after seeing it once
      localStorage.setItem('changelogSeen', currentMonth);
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
          <h2>What's New - {currentMonth}</h2>
        </Modal.Header>

        <p className="modal-body">{DESCRIPTION}</p>
        <img className="modal-img" src={IMAGE_URL} />

        <Row className="justify-content-center my-2">
          <Button className="py-2 px-4" variant="secondary" onClick={closeModal}>
            Close
          </Button>
        </Row>
      </Modal>
    </div>
  );
};

export default ChangelogModal;
