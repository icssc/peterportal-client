import { FC, useState } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import trpc from '../../../trpc';
import {
  addUncategorizedCourse,
  removeUncategorizedCourse,
  TransferWithUnread,
} from '../../../store/slices/transferCreditsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { TransferredUncategorized } from '@peterportal/types';

const UncategorizedMenuTile: FC<{ course: TransferWithUnread<TransferredUncategorized> }> = ({ course }) => {
  const { name, units, unread } = course;

  const dispatch = useAppDispatch();

  const deleteFn = () => {
    trpc.transferCredits.removeUncategorizedCourse.mutate({ name, units });
    dispatch(removeUncategorizedCourse({ name, units }));
  };

  return <MenuTile title={name ?? ''} units={units ?? 0} deleteFn={deleteFn} unread={unread} />;
};

const UncategorizedCreditInput: FC = () => {
  const dispatch = useAppDispatch();
  const [itemName, setItemName] = useState('');
  const [units, setUnits] = useState('');

  const updateItemName = (newName: string) => {
    setItemName(newName);
  };

  const updateUnits = (newUnits: string) => {
    if (newUnits === '' || /^\d*$/.test(newUnits)) {
      setUnits(newUnits);
    }
  };

  const handleSubmit = () => {
    dispatch(addUncategorizedCourse({ name: itemName, units: parseInt(units) }));
    setItemName('');
    setUnits('');
  };

  return (
    <div className="">
      <input value={itemName} onChange={(event) => updateItemName(event.target.value)} />
      <input value={units} onChange={(event) => updateUnits(event.target.value)} />
      <button onClick={handleSubmit}>Add</button>
    </div>
  );
};

const UncategorizedCreditsSection: FC = () => {
  const courses = useAppSelector((state) => state.transferCredits.uncategorizedCourses);

  return (
    <MenuSection title="Uncategorized Credits">
      <SectionDescription>
        These items were not automatically recognized as a course or AP Exam. Add the equivalent course or AP Exam
        manually, or leave these items as miscellaneous elective credits.
      </SectionDescription>

      {courses.map((course) => (
        <UncategorizedMenuTile key={`${course.name}-${course.units}`} course={course} />
      ))}

      <UncategorizedCreditInput />
    </MenuSection>
  );
};

export default UncategorizedCreditsSection;
