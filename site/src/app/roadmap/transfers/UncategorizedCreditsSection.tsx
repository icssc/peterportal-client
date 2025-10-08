import { FC, useState } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import { TextField, Button } from '@mui/material';
import trpc from '../../../trpc';
import {
  addUncategorizedCourse,
  updateUncategorizedCourse,
  removeUncategorizedCourse,
  TransferWithUnread,
} from '../../../store/slices/transferCreditsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { TransferredUncategorized } from '@peterportal/types';
import './UncategorizedCreditsSection.scss';

const UncategorizedMenuTile: FC<{ course: TransferWithUnread<TransferredUncategorized> }> = ({ course }) => {
  const { name, units, unread } = course;
  const dispatch = useAppDispatch();

  const setUnits = (newUnits: number) => {
    const updatedCourse: TransferredUncategorized = { name, units: newUnits };
    trpc.transferCredits.updateUncategorizedCourse.mutate(updatedCourse);
    dispatch(updateUncategorizedCourse(updatedCourse));
  };

  const deleteFn = () => {
    trpc.transferCredits.removeUncategorizedCourse.mutate({ name, units });
    dispatch(removeUncategorizedCourse({ name, units }));
  };

  return <MenuTile title={name ?? ''} units={units ?? 0} setUnits={setUnits} deleteFn={deleteFn} unread={unread} />;
};

const UncategorizedCreditInput: FC = () => {
  const dispatch = useAppDispatch();
  const [name, setName] = useState('');
  const [units, setUnits] = useState('');

  const updateName = (newName: string) => {
    if (newName === '' || newName.length <= 20) {
      setName(newName);
    }
  };

  const updateUnits = (newUnits: string) => {
    if (newUnits === '' || (parseFloat(newUnits) >= 0 && parseFloat(newUnits) < 1000)) {
      setUnits(newUnits);
    }
  };

  const handleSubmit = () => {
    if (name === '' || units === '') {
      return;
    }

    const newCredit: TransferredUncategorized = { name, units: parseFloat(units) };
    trpc.transferCredits.addUncategorizedCourse.mutate(newCredit);
    dispatch(addUncategorizedCourse(newCredit));

    setName('');
    setUnits('');
  };

  return (
    <div className="uncategorized-credit-input-row">
      <TextField
        className="name-input"
        required
        size="small"
        type="text"
        name="name"
        placeholder="Add name..."
        value={name}
        onChange={(e) => updateName(e.target.value)}
      />

      <TextField
        className="unit-input"
        required
        size="small"
        type="number"
        name="units"
        placeholder="Units..."
        value={units}
        onChange={(e) => updateUnits(e.target.value)}
      />

      <Button variant="contained" onClick={handleSubmit}>
        Add
      </Button>
    </div>
  );
};

const UncategorizedCreditsSection: FC = () => {
  const courses = useAppSelector((state) => state.transferCredits.uncategorizedCourses);

  return (
    <MenuSection title="Uncategorized Credits">
      <SectionDescription>
        These items were not automatically recognized as a course or AP Exam. Add the equivalent item manually, or leave
        these items as elective credits.
      </SectionDescription>

      {courses.map((course) => (
        <UncategorizedMenuTile key={`${course.name}-${course.units}`} course={course} />
      ))}

      <UncategorizedCreditInput />
    </MenuSection>
  );
};

export default UncategorizedCreditsSection;
