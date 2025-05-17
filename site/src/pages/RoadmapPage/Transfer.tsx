/** @deprecated This is the Legacy Transfers Menu component. It will be removed once the new menu shows missing prerequisites */
import { FC, useState, useEffect } from 'react';
import './Transfer.scss';
import Button from 'react-bootstrap/Button';
import { ListGroup } from 'react-bootstrap';
import Modal from 'react-bootstrap/Modal';
import Form from 'react-bootstrap/Form';
import Container from 'react-bootstrap/Container';
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';
import { PlusLg, Trash } from 'react-bootstrap-icons';

import { setShowTransfer, deleteTransfer, setTransfer, addTransfer } from '../../store/slices/roadmapSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { TransferData } from '@peterportal/types';

interface TransferEntryProps extends TransferData {
  index: number;
}

// a list of missing courses for current plan, passed into Transfer component to display dropdown
interface MissingCoursesProps {
  missingPrereqNames: Set<string>;
}

const TransferEntry: FC<TransferEntryProps> = (props) => {
  const dispatch = useAppDispatch();
  const [name, setName] = useState(props.name);
  const [units, setUnits] = useState(props.units);

  // when deleting a course, update new parameters
  useEffect(() => {
    setName(props.name);
    setUnits(props.units);
  }, [props.name, props.units]);

  // save to roadmap on every update
  useEffect(() => {
    dispatch(
      setTransfer({
        index: props.index,
        transfer: { name, units },
      }),
    );
  }, [dispatch, name, props.index, units]);

  return (
    <Row className="g-2 mb-1" xs={3}>
      <Col xs="8" md="7" className="p-0">
        <Form.Control type="text" placeholder="Name" value={name} onChange={(e) => setName(e.target.value)} />
      </Col>
      <Col xs="3" md="4" className="pr-0">
        <Form.Control
          type="number"
          placeholder="Units"
          value={units ?? undefined}
          onChange={(e) => setUnits(parseInt(e.target.value))}
        />
      </Col>
      <Col xs="1" md="1" className="entry-delete-icon">
        <Trash onClick={() => dispatch(deleteTransfer(props.index))} />
      </Col>
    </Row>
  );
};

/** @deprecated */
const Transfer: FC<MissingCoursesProps> = ({ missingPrereqNames }) => {
  const dispatch = useAppDispatch();
  const transfers = useAppSelector((state) => state.roadmap.transfers);
  const show = useAppSelector((state) => state.roadmap.showTransfer);
  const handleClose = () => dispatch(setShowTransfer(false));
  const DisplayMissingCourses: FC = () => {
    return (
      <ListGroup horizontal>
        {' '}
        {Array.from(missingPrereqNames).map((course) => (
          <ListGroup.Item key={course}>{course}</ListGroup.Item>
        ))}
      </ListGroup>
    );
  };

  return (
    <Modal className="transfer-modal ppc-modal" show={show} onHide={handleClose} centered>
      <Modal.Header closeButton>
        <h2>Transfer Credits</h2>
      </Modal.Header>
      <Modal.Body className="transfer-body">
        <p>
          Record your AP Credits or Community College Credits here. Doing so will clear the prerequisites on the
          roadmap.
        </p>
        <p>
          Notice: entered course names need to match exactly as displayed on the UCI catalog (eg. "AP computer science"
          must be entered as "AP COMP SCI A")
        </p>
        {missingPrereqNames?.size > 0 && (
          <>
            <p>Missing Prerequisites</p>
            <DisplayMissingCourses />
          </>
        )}
        <Container className="entry">
          <Form className="ppc-modal-form">
            {transfers.map((transfer, i) => (
              <TransferEntry key={`transfer-${i}`} index={i} {...transfer}></TransferEntry>
            ))}
          </Form>
        </Container>
      </Modal.Body>
      <Modal.Footer className="transfer-footer d-flex flex-row justify-content-between pt-0">
        <Button
          className="add-entry"
          variant="none"
          onClick={() => dispatch(addTransfer({ name: '', units: undefined }))}
        >
          <PlusLg /> Add Entry
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default Transfer;
