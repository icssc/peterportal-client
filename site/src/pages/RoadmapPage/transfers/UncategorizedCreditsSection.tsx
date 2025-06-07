import { FC } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import trpc from '../../../trpc';
import { removeUncategorizedCourse, TransferWithUnread } from '../../../store/slices/transferCreditsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { TransferredUncategorized } from '@peterportal/types';

const UncategorizedMenuTile: FC<TransferWithUnread<TransferredUncategorized>> = ({ name, units, unread }) => {
  const dispatch = useAppDispatch();

  const deleteFn = () => {
    trpc.transferCredits.removeUncategorizedCourse.mutate({ name, units });
    dispatch(removeUncategorizedCourse({ name, units }));
  };

  return <MenuTile title={name ?? ''} units={units ?? 0} deleteFn={deleteFn} unread={unread} />;
};

const UncategorizedCreditsSection: FC = () => {
  const courses = useAppSelector((state) => state.transferCredits.uncategorizedCourses);

  if (courses.length === 0) {
    return null;
  }

  return (
    <MenuSection title="Other Transferred Credits">
      <SectionDescription>
        These items were not automatically recognized as a course or AP Exam. Once you add equivalent credits manually,
        you can remove them.
      </SectionDescription>

      {courses.map((course) => (
        <UncategorizedMenuTile
          key={`${course.name}-${course.units}`}
          name={course.name}
          units={course.units}
          unread={course.unread}
        />
      ))}
    </MenuSection>
  );
};

export default UncategorizedCreditsSection;
