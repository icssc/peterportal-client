import { useEffect, useState } from 'react';
import './ChangelogModal.scss';
import Button from 'react-bootstrap/Button';
import Modal from 'react-bootstrap/Modal';
import Row from 'react-bootstrap/Row';

const DESCRIPTION = 'You can now view recently added features to the PeterPortal website, listed in this modal.';
const IMAGE_URL = 'https://media.tenor.com/FMJCWGaIwT0AAAAM/cat-thumbs-up.gif';

const ChangelogModal = () => {
  const [showModal, setShowModal] = useState(false);

  useEffect(() => {
    // display the changelog modal if it is the user's first time seeing it (tracked in local storage)
    let seen = localStorage.getItem('changelogSeen');
    if (seen === null) {
      setShowModal(true);

      // mark as seen so it is not displayed after seeing it once
      localStorage.setItem('changelogSeen', '1');
    }
  }, []);

  const closeModal = () => {
    setShowModal(false);
  };

  return (
    <Modal className="modal-card" show={showModal} centered animation={false}>
      <h2 className="modal-header">
        What's New - {new Date().toLocaleString('default', { month: 'long', year: 'numeric' })}
      </h2>

      <p className="modal-body">{DESCRIPTION}</p>

      <img className="modal-img" src={IMAGE_URL} />

      <p className="modal-body">
        If you have any feedback or features you would like to see, check out the feedback form!
      </p>

      <Row className="justify-content-center">
        <Button className="py-2 px-4" variant="outline-secondary" onClick={closeModal}>
          Close
        </Button>
      </Row>
    </Modal>
  );
};

export default ChangelogModal;
