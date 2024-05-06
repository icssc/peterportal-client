// import Button from '@mui/material/Button';
import Dialog from '@mui/material/Dialog';
import DialogContent from '@mui/material/DialogContent';
import DialogTitle from '@mui/material/DialogTitle';
import TextField from '@mui/material/TextField';
import { FC, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import {
  addRoadmapPlan,
  deleteRoadmapPlan,
  initialPlanState,
  RoadmapPlan,
  setPlanIndex,
  setPlanName,
} from '../../store/slices/roadmapSlice';
import { Box } from '@mui/material';
import Dropdown from 'react-bootstrap/esm/Dropdown';
import './RoadmapMultiplan.scss';
import * as Icon from 'react-bootstrap-icons';
import { Button } from 'semantic-ui-react';

interface RoadmapSelectableItemProps {
  plan: RoadmapPlan;
  index: number;
  clickHandler: () => void;
  editHandler: () => void;
  deleteHandler: () => void;
}

const RoadmapSelectableItem: FC<RoadmapSelectableItemProps> = ({
  plan,
  index,
  clickHandler,
  editHandler,
  deleteHandler,
}) => {
  return (
    <div className="select-item">
      <Dropdown.Item key={plan.name} value={index} onClick={clickHandler}>
        <Button>{plan.name}</Button>
      </Dropdown.Item>
      <Button variant={'dark'} onClick={editHandler}>
        <Icon.PencilFill width="16" height="16" />
      </Button>
      <Button variant={''} onClick={deleteHandler}>
        <Icon.TrashFill width="16" height="16" />
      </Button>
    </div>
  );
};

const RoadmapMultiplan: FC = () => {
  const dispatch = useAppDispatch();
  const allPlans = useAppSelector((state) => state.roadmap);
  const [currentPlanIndex, setCurrentPlanIndex] = useState(allPlans.currentPlanIndex);
  const [isOpen, setIsOpen] = useState(false);
  const [isEdit, setIsEdit] = useState(false);
  const [isDelete, setIsDelete] = useState(false);
  const [newPlanName, setNewPlanName] = useState(allPlans.plans[allPlans.currentPlanIndex].name);
  const [showDropdown, setShowDropdown] = useState(false);

  // name: name of the plan, content: stores the content of plan
  // const { name, content } = allPlans.plans[currentPlanIndex];
  const { name } = allPlans.plans[currentPlanIndex];

  const addNewPlan = (name: string) => {
    dispatch(addRoadmapPlan({ name: name, content: initialPlanState }));
  };

  const deleteCurrentPlan = () => {
    setCurrentPlanIndex(0);
    dispatch(setPlanIndex(0));
    dispatch(deleteRoadmapPlan({ planIndex: currentPlanIndex }));
    setIsDelete(false);
  };

  const handleSubmitNewPlan = () => {
    setIsOpen(false);
    addNewPlan(newPlanName);
    const newIndex = allPlans.plans.length;
    setCurrentPlanIndex(newIndex);
    dispatch(setPlanIndex(newIndex));
  };

  const modifyPlanName = () => {
    setIsEdit(false);
    dispatch(setPlanName({ index: allPlans.currentPlanIndex, name: newPlanName }));
  };

  return (
    <div className="multi-plan-selector">
      <Dropdown show={showDropdown} onToggle={(s) => setShowDropdown(s)}>
        <Dropdown.Toggle id="dropdown-basic">
          <span>{name}</span> {/** @todo this is an active title */}
        </Dropdown.Toggle>
        <Dropdown.Menu>
          {allPlans.plans.map((plan, index) => (
            <RoadmapSelectableItem
              key={plan.name}
              plan={plan}
              index={index}
              clickHandler={() => {
                dispatch(setPlanIndex(index));
                setCurrentPlanIndex(index);
              }}
              editHandler={() => setIsEdit(true)}
              deleteHandler={() => setIsDelete(true)}
            />
          ))}
          {/* <Dropdown.Item onClick={() => }>Edit Plan Name</Dropdown.Item> */}
          <div className="select-item">
            <Dropdown.Item onClick={() => setIsOpen(true)}>
              <Button>Add Plan</Button>
            </Dropdown.Item>
          </div>
        </Dropdown.Menu>
      </Dropdown>
      {/* <InputLabel style={{ width: 'min-content' }} id="demo-simple-select-standard-label">
            {name}
          </InputLabel> */}
      {/* A plan selector I guess???? */}
      {/* <Select
            labelId="demo-simple-select-standard-label"
            id="demo-simple-select-standard"
            sx={{ width: 'auto' }}
            value={''}
            label={name}
            onChange={(e) => {
              const newIndex = +e.target.value;
              // console.log("selected index", newIndex);
              dispatch(setPlanIndex(newIndex));
              setCurrentPlanIndex(newIndex);
            }}
          >
            {allPlans.plans.map((plan, index) => {
              return (
                <MenuItem key={plan.name} value={index}>
                  {plan.name}
                </MenuItem>
              );
            })}
          </Select> */}
      <Dialog
        open={isOpen}
        onClose={() => setIsOpen(false)}
        PaperProps={{ sx: { width: '30%', height: '20%' } }}
        style={{ marginTop: 20 }}
      >
        <DialogTitle>New Plan</DialogTitle>
        <DialogContent>
          <TextField
            margin="dense"
            id="name"
            label="Plan Name"
            type="email"
            fullWidth
            variant="standard"
            placeholder="plan name"
            onChange={(e) => {
              setNewPlanName(e.target.value);
            }}
            style={{ width: '100%' }}
          />
          <Button style={{ float: 'right' }} onClick={() => handleSubmitNewPlan()}>
            Submit
          </Button>
        </DialogContent>
      </Dialog>
      {/* Edit Plan Name */}
      <Dialog
        open={isEdit}
        onClose={() => setIsEdit(false)}
        PaperProps={{ sx: { width: '30%', height: '20%' } }}
        style={{ marginTop: 20 }}
      >
        <DialogTitle>Edit Plan Name</DialogTitle>
        <DialogContent>
          <TextField
            margin="dense"
            id="name"
            label=""
            type="email"
            fullWidth
            variant="standard"
            placeholder=""
            defaultValue={allPlans.plans[allPlans.currentPlanIndex].name}
            onChange={(e) => {
              setNewPlanName(e.target.value);
            }}
            style={{ width: '100%' }}
          />
          <Button style={{ float: 'right' }} onClick={() => modifyPlanName()}>
            Submit
          </Button>
        </DialogContent>
      </Dialog>
      {/* Delete dialog */}
      <Dialog
        open={isDelete}
        onClose={() => setIsDelete(false)}
        PaperProps={{ sx: { width: '30%', height: '20%' } }}
        style={{ marginTop: 20 }}
      >
        <DialogTitle>Delete Plan</DialogTitle>
        <DialogContent>
          <p>Are you sure about deleting current plan?</p>
          <Box sx={{ display: 'flex', flexDirection: 'row', justifyContent: 'flex-end' }}>
            <Button onClick={() => setIsDelete(false)}>Cancel</Button>
            <Button sx={{ color: 'red' }} onClick={() => deleteCurrentPlan()}>
              Delete
            </Button>
          </Box>
        </DialogContent>
      </Dialog>
      {/* <Button
        style={{ alignItems: 'center', color: 'red', marginTop: 10, height: 30 }}
        onClick={() => setIsDelete(true)}
      >
        Delete Plan
      </Button> */}
    </div>
  );
};

export default RoadmapMultiplan;
