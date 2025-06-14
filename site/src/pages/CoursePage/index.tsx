import { FC, useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';

import GradeDist from '../../component/GradeDist/GradeDist';
import PrereqTree from '../../component/PrereqTree/PrereqTree';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';
import { ResultPageContent, ResultPageSection } from '../../component/ResultPageContent/ResultPageContent';

import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import { getCourseTags, searchAPIResult, sortTerms } from '../../helpers/util';

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

  const courseName = courseGQLData.department + ' ' + courseGQLData.courseNumber;

  const sideInfo = (
    <SideInfo
      dataType="course"
      data={courseGQLData}
      name={courseName}
      title={courseGQLData.title}
      description={courseGQLData.description}
      tags={getCourseTags(courseGQLData)}
      terms={courseGQLData.terms}
    />
  );

  const mainContent = (
    <>
      <ResultPageSection title="📊 Grade Distribution">
        <GradeDist dataType="course" data={courseGQLData} />
      </ResultPageSection>

      <ResultPageSection title="🌲 Prerequisite Tree">
        <PrereqTree course={courseGQLData} />
      </ResultPageSection>

      <ResultPageSection title="🗓️ Schedule of Classes">
        <Schedule course={courseGQLData} terms={sortTerms(courseGQLData.terms)} />
      </ResultPageSection>

      <ResultPageSection title="💬 Reviews">
        <Review course={courseGQLData} terms={sortTerms(courseGQLData.terms)} />
      </ResultPageSection>
    </>
  );

  return <ResultPageContent sideInfo={sideInfo} mainContent={mainContent} />;
};

export default CoursePage;
