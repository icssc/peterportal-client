import Button from '@mui/material/Button';
import Dialog from '@mui/material/Dialog';
import DialogContent from '@mui/material/DialogContent';
import DialogTitle from '@mui/material/DialogTitle';
import FormControl from '@mui/material/FormControl';
import InputLabel from '@mui/material/InputLabel';
import MenuItem from '@mui/material/MenuItem';
import Select from '@mui/material/Select';
import TextField from '@mui/material/TextField';
import { FC, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import {
  addRoadmapPlan,
  deleteRoadmapPlan,
  initialState,
  setPlanIndex,
  setPlanName,
} from '../../store/slices/roadmapSlice';
import { Box } from '@mui/material';
import Dropdown from 'react-bootstrap/esm/Dropdown';
import './RoadmapMultiplan.scss';
import * as Icon from 'react-bootstrap-icons';

const RoadmapMultiplan: FC = () => {
  const dispatch = useAppDispatch();
  const allPlans = useAppSelector((state) => state.roadmap);
  const [currentPlanIndex, setCurrentPlanIndex] = useState(allPlans.currentPlanIndex);
  const [isOpen, setIsOpen] = useState(false);
  const [isEdit, setIsEdit] = useState(false);
  const [isDelete, setIsDelete] = useState(false);
  const [newPlanName, setNewPlanName] = useState(allPlans.plans[allPlans.currentPlanIndex].name);

  // name: name of the plan, content: stores the content of plan
  // const { name, content } = allPlans.plans[currentPlanIndex];
  const { name } = allPlans.plans[currentPlanIndex];

  const addNewPlan = (name: string) => {
    dispatch(addRoadmapPlan({ name: name, content: initialState }));
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
      <Box sx={{ display: 'flex', gap: 2, flexDirection: 'row', alignItems: 'center' }}>
        <Dropdown>
          <Dropdown.Toggle id="dropdown-basic">
            <Icon.PenFill style={{ width: '50%', height: '50%', marginRight: 5 }} />
          </Dropdown.Toggle>
          <Dropdown.Menu>
            <Dropdown.Item onClick={() => setIsEdit(true)}>Edit Plan Name</Dropdown.Item>
            <Dropdown.Item onClick={() => setIsOpen(true)}>Add Plan</Dropdown.Item>
          </Dropdown.Menu>
        </Dropdown>
        <FormControl variant="standard" sx={{ m: 1, minWidth: 120, marginBottom: 3 }}>
          <InputLabel style={{ width: 'min-content' }} id="demo-simple-select-standard-label">
            {name}
          </InputLabel>
          <Select
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
          </Select>
          <Dialog
            open={isOpen}
            onClose={() => setIsOpen(false)}
            PaperProps={{ sx: { width: '30%', height: '20%' } }}
            style={{ marginTop: 20 }}
          >
            <DialogTitle>New Plan</DialogTitle>
            <DialogContent>
              <TextField
                autoFocus
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
          <Dialog
            open={isEdit}
            onClose={() => setIsEdit(false)}
            PaperProps={{ sx: { width: '30%', height: '20%' } }}
            style={{ marginTop: 20 }}
          >
            <DialogTitle>Edit Plan Name</DialogTitle>
            <DialogContent>
              <TextField
                autoFocus
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
        </FormControl>
      </Box>
      <Button
        style={{ alignItems: 'center', color: 'red', marginTop: 10, height: 30 }}
        onClick={() => setIsDelete(true)}
      >
        Delete Plan
      </Button>
    </div>
  );
};

export default RoadmapMultiplan;
