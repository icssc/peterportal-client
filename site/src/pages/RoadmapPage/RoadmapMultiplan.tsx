import React, { FC, ReactNode, useEffect, useRef, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import {
  addRoadmapPlan,
  defaultPlan,
  deleteRoadmapPlan,
  initialPlanState,
  RoadmapPlan,
  setPlanIndex,
  setPlanName,
} from '../../store/slices/roadmapSlice';
import './RoadmapMultiplan.scss';
import { Button as Button2, Form, Modal } from 'react-bootstrap';
import { makeUniquePlanName } from '../../helpers/planner';
import spawnToast from '../../helpers/toastify';
import ImportTranscriptPopup from './ImportTranscriptPopup';
import ImportZot4PlanPopup from './ImportZot4PlanPopup';

import EditIcon from '@mui/icons-material/Edit';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import AddIcon from '@mui/icons-material/Add';
import ContentCopyOutlinedIcon from '@mui/icons-material/ContentCopyOutlined';
import { Button, IconButton, Popover } from '@mui/material';

import ArrowDropDownIcon from '@mui/icons-material/ArrowDropDown';

interface RoadmapSelectableItemProps {
  plan: RoadmapPlan;
  index: number;
  clickHandler: () => void;
  editHandler: () => void;
  duplicateHandler: () => void;
  deleteHandler: () => void;
}

const RoadmapSelectableItem: FC<RoadmapSelectableItemProps> = ({
  plan,
  clickHandler,
  editHandler,
  duplicateHandler,
  deleteHandler,
}) => {
  return (
    <div className="select-item">
      <Button variant="text" className="planner-name-btn" onClick={clickHandler}>
        {plan.name}
      </Button>
      <IconButton onClick={editHandler}>
        <EditIcon />
      </IconButton>
      <IconButton onClick={duplicateHandler}>
        <ContentCopyOutlinedIcon />
      </IconButton>
      <IconButton className="delete-btn" onClick={deleteHandler}>
        <DeleteOutlineIcon />
      </IconButton>
    </div>
  );
};

interface MultiplanDropdownProps {
  setEditIndex: (index: number) => void;
  setDeleteIndex: (index: number) => void;
  handleCreate: () => void;
  children?: ReactNode;
}
const MultiplanDropdown: FC<MultiplanDropdownProps> = ({ children, setEditIndex, setDeleteIndex, handleCreate }) => {
  const dispatch = useAppDispatch();
  const allPlans = useAppSelector((state) => state.roadmap);
  const currentPlanIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const { name } = allPlans.plans[currentPlanIndex];
  const [showDropdown, setShowDropdown] = useState(false);
  const containerRef = useRef<HTMLDivElement | null>(null);

  const duplicatePlan = (plan: RoadmapPlan) => {
    const newName = makeUniquePlanName(plan.name, allPlans.plans);
    dispatch(
      addRoadmapPlan({
        name: newName,
        content: JSON.parse(JSON.stringify(plan.content)),
      }),
    );
    const newIndex = allPlans.plans.length;
    dispatch(setPlanIndex(newIndex));
  };

  const handleClose = (_: Event, reason?: string) => {
    const multiplanModalOpen = !!document.querySelector('.multiplan-modal');
    if (reason === 'escapeKeyDown' && multiplanModalOpen) return;
    setShowDropdown(false);
  };

  return (
    <div ref={containerRef}>
      <Button
        className="dropdown-button"
        variant="outlined"
        onClick={() => setShowDropdown(!showDropdown)}
        endIcon={<ArrowDropDownIcon />}
      >
        {name}
      </Button>
      <Popover
        className="multi-plan-selector"
        open={showDropdown}
        anchorReference="anchorEl"
        anchorEl={containerRef.current}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'left' }}
        onClose={handleClose}
      >
        {allPlans.plans.map((plan, index) => (
          <RoadmapSelectableItem
            key={plan.name}
            plan={plan}
            index={index}
            clickHandler={() => {
              dispatch(setPlanIndex(index));
            }}
            editHandler={() => setEditIndex(index)}
            duplicateHandler={() => duplicatePlan(plan)}
            deleteHandler={() => setDeleteIndex(index)}
          />
        ))}
        <div className="separator-label">
          Add or Import Roadmap
          <hr />
        </div>
        <div className="select-item add-item">
          <Button variant="text" onClick={handleCreate}>
            <AddIcon />
            <span>Blank Roadmap</span>
          </Button>
          <ImportTranscriptPopup />
          <ImportZot4PlanPopup />
        </div>
        {children}
      </Popover>
    </div>
  );
};

const RoadmapMultiplan: FC = () => {
  const dispatch = useAppDispatch();
  const allPlans = useAppSelector((state) => state.roadmap);
  const currentPlanIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const [isOpen, setIsOpen] = useState(false);
  const [editIdx, setEditIdx] = useState(-1);
  const [delIdx, setDelIdx] = useState(-1);
  const [newPlanName, setNewPlanName] = useState(allPlans.plans[allPlans.currentPlanIndex].name);
  const isDuplicateName = () => allPlans.plans.find((p) => p.name === newPlanName);

  const name = allPlans.plans[currentPlanIndex].name;

  const addNewPlan = (name: string) => {
    dispatch(addRoadmapPlan({ name: name, content: initialPlanState }));
  };

  const deleteCurrentPlan = () => {
    const newIndex = delIdx === currentPlanIndex ? 0 : currentPlanIndex - Number(delIdx < currentPlanIndex);
    dispatch(setPlanIndex(newIndex));
    dispatch(deleteRoadmapPlan({ planIndex: delIdx }));
    setDelIdx(-1);
  };

  const handleSubmitNewPlan = () => {
    if (!newPlanName) return spawnToast('Name cannot be empty', true);
    if (isDuplicateName()) return spawnToast('A plan with that name already exists', true);
    setIsOpen(false);
    addNewPlan(newPlanName);
    const newIndex = allPlans.plans.length;
    dispatch(setPlanIndex(newIndex));
  };

  const modifyPlanName = () => {
    if (!newPlanName) return spawnToast('Name cannot be empty', true);
    if (isDuplicateName()) return spawnToast('A plan with that name already exists', true);
    dispatch(setPlanName({ index: editIdx, name: newPlanName }));
    setEditIdx(-1);
  };

  useEffect(() => {
    document.title = `${name} | PeterPortal`;
  }, [name]);

  return (
    <MultiplanDropdown handleCreate={() => setIsOpen(true)} setEditIndex={setEditIdx} setDeleteIndex={setDelIdx}>
      {/* Create Roadmap Modal */}
      <Modal
        show={isOpen}
        onShow={() => {
          setIsOpen(true);
          const planCount = allPlans.plans?.length ?? 0;
          let newIdx = planCount + 1;
          while (allPlans.plans.find((p) => p.name === `Roadmap ${newIdx}`)) newIdx++;
          setNewPlanName(`Roadmap ${newIdx}`);
        }}
        onHide={() => setIsOpen(false)}
        centered
        className="ppc-modal multiplan-modal"
      >
        <Modal.Header closeButton>
          <h2>New Roadmap</h2>
        </Modal.Header>
        <Modal.Body>
          <Form noValidate className="ppc-modal-form">
            <Form.Group>
              <Form.Label className="ppc-modal-form-label">Roadmap Name</Form.Label>
              <Form.Control
                required
                type="text"
                name="roadmap_name"
                value={newPlanName}
                onChange={(e) => setNewPlanName(e.target.value)}
                onKeyDown={(e: React.KeyboardEvent) => {
                  // prevent submitting form (reloads the page)
                  if (e.key === 'Enter') e.preventDefault();
                }}
                maxLength={35}
                placeholder={defaultPlan.name}
              ></Form.Control>
            </Form.Group>
          </Form>
          <Button2
            variant="primary"
            onClick={() => {
              handleSubmitNewPlan();
            }}
          >
            Create Roadmap
          </Button2>
        </Modal.Body>
      </Modal>

      {/* Edit Roadmap Modal */}
      <Modal
        show={editIdx !== -1}
        onShow={() => {
          setNewPlanName(allPlans.plans[editIdx].name);
        }}
        onHide={() => setEditIdx(-1)}
        centered
        className="ppc-modal multiplan-modal"
      >
        <Modal.Header closeButton>
          <h2>Edit Roadmap</h2>
        </Modal.Header>
        <Modal.Body>
          <Form noValidate className="ppc-modal-form">
            <Form.Group>
              <Form.Label className="ppc-modal-form-label">Roadmap Name</Form.Label>
              <Form.Control
                required
                type="text"
                name="roadmap_name"
                value={newPlanName}
                onChange={(e) => setNewPlanName(e.target.value)}
                onKeyDown={(e: React.KeyboardEvent) => {
                  // prevent submitting form (reloads the page)
                  if (e.key === 'Enter') e.preventDefault();
                }}
                maxLength={35}
                placeholder={defaultPlan.name}
              ></Form.Control>
            </Form.Group>
          </Form>
          <Button2
            variant="primary"
            onClick={() => {
              modifyPlanName();
            }}
          >
            Save Roadmap
          </Button2>
        </Modal.Body>
      </Modal>

      {/* Delete Roadmap Modal */}
      <Modal
        show={delIdx !== -1}
        onShow={() => {
          setNewPlanName(allPlans.plans[delIdx].name);
        }}
        onHide={() => setDelIdx(-1)}
        centered
        className="ppc-modal multiplan-modal"
      >
        <Modal.Header closeButton>
          <h2>Delete Roadmap</h2>
        </Modal.Header>
        <Modal.Body>
          <Form noValidate className="ppc-modal-form">
            <Form.Group>
              <p>Are you sure you want to delete the roadmap "{newPlanName}"?</p>
            </Form.Group>
          </Form>
          <Button2
            variant="danger"
            onClick={() => {
              deleteCurrentPlan();
            }}
          >
            I am sure
          </Button2>
        </Modal.Body>
      </Modal>
    </MultiplanDropdown>
  );
};

export default RoadmapMultiplan;
