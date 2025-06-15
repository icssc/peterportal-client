import { FC, useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';

import GradeDist from '../../component/GradeDist/GradeDist';
import PrereqTree from '../../component/PrereqTree/PrereqTree';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';
import ResultPageContent from '../../component/ResultPageContent/ResultPageContent';

import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { getCourseTags, searchAPIResult } from '../../helpers/util';
import { Section } from '../../types/types';

const CoursePage: FC = () => {
  const { id } = useParams<{ id: string }>();
  const dispatch = useAppDispatch();
  const courseGQLData = useAppSelector((state) => state.popup.course);
  const [error, setError] = useState('');

  useEffect(() => {
    if (!id) return;

    searchAPIResult('course', id).then((course) => {
      if (!course) {
        setError(`Course ${id} does not exist!`);
        return;
      }
      dispatch(setCourse(course));
      setError('');
      document.title = `${course.department + ' ' + course.courseNumber} | PeterPortal`;
    });
  }, [dispatch, id]);

  // if course does not exists
  if (error) {
    return <Error message={error} />;
  }

  // loading results
  if (!courseGQLData) {
    return <LoadingSpinner />;
  }

  const sideInfo = (
    <SideInfo
      dataType="course"
      data={courseGQLData}
      name={`${courseGQLData.department} ${courseGQLData.courseNumber}`}
      title={courseGQLData.title}
      description={courseGQLData.description}
      tags={getCourseTags(courseGQLData)}
      terms={courseGQLData.terms}
    />
  );

  const mainSections: Section[] = [
    { title: '📊 Grade Distribution', Component: GradeDist },
    { title: '🌲 Prerequisite Tree', Component: PrereqTree },
    { title: '🗓️ Schedule of Classes', Component: Schedule },
    { title: '💬 Reviews', Component: Review },
  ];

  return <ResultPageContent dataType="course" data={courseGQLData} sideInfo={sideInfo} mainSections={mainSections} />;
};

export default CoursePage;
