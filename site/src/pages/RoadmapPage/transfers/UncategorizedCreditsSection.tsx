import { FC } from 'react';
import MenuSection, { SectionDescription } from './MenuSection';
import MenuTile from './MenuTile';
import trpc from '../../../trpc';
import { removeUncategorizedCourse } from '../../../store/slices/transferCreditsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { TransferredUncategorized } from '@peterportal/types';

const UncategorizedMenuTile: FC<{ uncategorizedInfo: TransferredUncategorized; unread: boolean }> = ({
  uncategorizedInfo,
  unread,
}) => {
  const dispatch = useAppDispatch();

  const deleteFn = () => {
    trpc.transferCredits.removeUncategorizedCourse.mutate({
      name: uncategorizedInfo.name,
      units: uncategorizedInfo.units,
    });
    dispatch(removeUncategorizedCourse({ name: uncategorizedInfo.name, units: uncategorizedInfo.units }));
  };

  return (
    <MenuTile
      title={uncategorizedInfo.name ?? ''}
      units={uncategorizedInfo.units ?? 0}
      deleteFn={deleteFn}
      unread={unread}
    />
  );
};

const UncategorizedCreditsSection: FC = () => {
  const courses = useAppSelector((state) => state.transferCredits.uncategorizedCourses);
  const unreadOther = useAppSelector((state) => state.transferCredits.unreadTransfers).otherNames;

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
          uncategorizedInfo={course}
          unread={course.name ? unreadOther.includes(course.name) : false}
        />
      ))}
    </MenuSection>
  );
};

export default UncategorizedCreditsSection;
