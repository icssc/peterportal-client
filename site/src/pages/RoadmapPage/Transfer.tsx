import React, { FC, useState, useEffect } from "react";
import './Transfer.scss';
import Button from 'react-bootstrap/Button';
import Modal from 'react-bootstrap/Modal';
import Form from 'react-bootstrap/Form';
import Container from 'react-bootstrap/Container';
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';
import CloseButton from 'react-bootstrap/CloseButton';
import { Pencil, Save } from "react-bootstrap-icons";

import { TransferData } from '../../types/types';
import { setShowTransfer, deleteTransfer, setTransfer, addTransfer } from '../../store/slices/roadmapSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';

interface TransferEntryProps extends TransferData {
    index: number;
}

const TransferEntry: FC<TransferEntryProps> = (props) => {
    const dispatch = useAppDispatch();
    const [name, setName] = useState(props.name);
    const [units, setUnits] = useState(props.units);

    // when deleting a course, update new parameters
    useEffect(() => {
        setName(props.name);
        setUnits(props.units);
    }, [props.name, props.units])

    // save to roadmap on every update
    useEffect(() => {
        dispatch(setTransfer({
            index: props.index,
            transfer: { name, units }
        }))
    }, [name, units])

    return <Row className="g-2 mb-1">
        <Col md='auto' className="d-flex flex-row justify-content-center">
            <CloseButton onClick={() => dispatch(deleteTransfer(props.index))} />
        </Col>
        <Col md>
            <Form.Control type="text" placeholder="Name"
                value={name} onChange={e =>
                    setName(e.target.value)
                } />
        </Col>
        <Col md>
            <Form.Control type="number" placeholder="Units"
                value={units} onChange={e =>
                    setUnits(parseInt(e.target.value))
                } />
        </Col>
    </Row>
}

const Transfer: FC = () => {
    const dispatch = useAppDispatch();
    const transfers = useAppSelector(state => state.roadmap.transfers);
    const show = useAppSelector(state => state.roadmap.showTransfer);
    const handleClose = () => dispatch(setShowTransfer(false));

    return (
        <Modal show={show} onHide={handleClose} centered>
            <Modal.Header closeButton>
                <Modal.Title>Transfer Credits</Modal.Title>
            </Modal.Header>
            <Modal.Body className='transfer' >
                <p>Record your AP Credits or Community College Credits here. Doing so will clear the prerequisites on the roadmap.</p>
                <Container>
                    <Form>
                        {
                            transfers.map((transfer, i) => <TransferEntry key={`transfer-${i}`} index={i} {...transfer}></TransferEntry>)
                        }
                    </Form>
                </Container>
            </Modal.Body>
            <Modal.Footer className='d-flex flex-row justify-content-between'>
                <Button variant="primary" onClick={() => dispatch(addTransfer({ name: '', units: undefined }))}>
                    Add Entry
                </Button>
                <Button variant="secondary" onClick={handleClose}>
                    Close
                </Button>
            </Modal.Footer>
        </Modal>
    );
}

export default Transfer;