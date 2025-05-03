import { FC, useContext, useEffect, useState } from 'react';
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
import * as Icon from 'react-bootstrap-icons';
import { Button, Dropdown, Form, Modal } from 'react-bootstrap';
import { makeUniquePlanName } from '../../helpers/planner';
import spawnToast from '../../helpers/toastify';
import ThemeContext from '../../style/theme-context';
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
  index,
  clickHandler,
  editHandler,
  duplicateHandler,
  deleteHandler,
}) => {
  const { darkMode } = useContext(ThemeContext);
  const buttonVariant = darkMode ? 'dark' : 'light';
  return (
    <div className="select-item">
      <Dropdown.Item key={plan.name} value={index} onClick={clickHandler}>
        <Button variant={buttonVariant} className="planner-name-btn">
          {plan.name}
        </Button>
      </Dropdown.Item>
      <Button variant={buttonVariant} onClick={editHandler}>
        <Icon.PencilFill width="16" height="16" />
      </Button>
      <Button variant={buttonVariant} onClick={duplicateHandler}>
        <Icon.Files width="16" height="16" />
      </Button>
      <Button variant={buttonVariant} onClick={deleteHandler}>
        <Icon.TrashFill width="16" height="16" />
      </Button>
    </div>
  );
};

const RoadmapMultiplan: FC = () => {
  const dispatch = useAppDispatch();
  const allPlans = useAppSelector((state) => state.roadmap);
  const currentPlanIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const { darkMode } = useContext(ThemeContext);
  const [isOpen, setIsOpen] = useState(false);
  const [editIdx, setEditIdx] = useState(-1);
  const [delIdx, setDelIdx] = useState(-1);
  const [newPlanName, setNewPlanName] = useState(allPlans.plans[allPlans.currentPlanIndex].name);
  const [showDropdown, setShowDropdown] = useState(false);
  const isDuplicateName = () => allPlans.plans.find((p) => p.name === newPlanName);

  // name: name of the plan, content: stores the content of plan
  // const { name, content } = allPlans.plans[currentPlanIndex];
  const { name } = allPlans.plans[currentPlanIndex];

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

  useEffect(() => {
    document.title = `${name} | PeterPortal`;
  }, [name]);

  return (
    <div className="multi-plan-selector">
      <Dropdown
        show={showDropdown}
        onToggle={(s, e) => {
          const target = e.target as HTMLElement;
          // Stay open if something in this menu is clicked unless it's switching roadmaps
          const inMenu =
            document.querySelector('.multi-plan-selector')!.contains(target) &&
            !target.classList.contains('planner-name-btn');
          const inDialog = document.querySelector('.multiplan-modal')?.contains(target) ?? false;
          setShowDropdown(s || inMenu || inDialog);
        }}
      >
        <Dropdown.Toggle onClick={() => setShowDropdown(!showDropdown)}>
          <span>{name}</span>
        </Dropdown.Toggle>
        <Dropdown.Menu>
          {allPlans.plans.map((plan, index) => (
            <RoadmapSelectableItem
              key={plan.name}
              plan={plan}
              index={index}
              clickHandler={() => {
                dispatch(setPlanIndex(index));
              }}
              editHandler={() => setEditIdx(index)}
              duplicateHandler={() => duplicatePlan(plan)}
              deleteHandler={() => setDelIdx(index)}
            />
          ))}
          <div className="select-item add-item">
            <Dropdown.Item onClick={() => setIsOpen(true)}>
              <Button variant={darkMode ? 'dark' : 'light'}>
                <Icon.PlusLg width="16" height="16" />
                New Roadmap
              </Button>
            </Dropdown.Item>
          </div>
        </Dropdown.Menu>
      </Dropdown>

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
          <Button
            variant="primary"
            onClick={() => {
              handleSubmitNewPlan();
            }}
          >
            Create Roadmap
          </Button>
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
          <Button
            variant="primary"
            onClick={() => {
              modifyPlanName();
            }}
          >
            Save Roadmap
          </Button>
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
          <Button
            variant="danger"
            onClick={() => {
              deleteCurrentPlan();
            }}
          >
            I am sure
          </Button>
        </Modal.Body>
      </Modal>
    </div>
  );
};

export default RoadmapMultiplan;
