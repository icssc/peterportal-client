import { FC } from 'react';
import './ReviewForm.scss';
import Modal from 'react-bootstrap/Modal';

interface ChangelogModalProps {
  closeForm: () => void;
}

const ChangelogModal: FC<ChangelogModalProps> = (props) => {
  return (
    <Modal onHide={props.closeForm} centered animation={false}>
      <h2>What's New - {new Date().toLocaleString('default', { month: 'long', year: 'numeric' })}</h2>

      <ul>
        <li>Thing 1</li>
        <li>Thing 2</li>
      </ul>

      <img src="" />

      <p>If you have any feedback or features you would like to see, check out the feedback form!</p>
    </Modal>
  );
};

export default ChangelogModal;
