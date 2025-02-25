import { useEffect, useState } from 'react';
import './ChangelogModal.scss';
import Modal from 'react-bootstrap/Modal';
import changelogImage1 from '../../asset/degree-reqs-major.png';
import changelogImage2 from '../../asset/degree-reqs-minor.png';

const DESCRIPTION = 'You can now view completion of major, minor, and GE requirements on your roadmap';
const LAST_UPDATED = '2/25/2025';

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
      <div className="modal-side-by-side">
        <img className="modal-img" src={changelogImage1} alt="Screenshot or gif of new changes" />
        <img className="modal-img" src={changelogImage2} alt="Screenshot or gif of new changes" />
      </div>
    </Modal>
  );
};

export default ChangelogModal;
