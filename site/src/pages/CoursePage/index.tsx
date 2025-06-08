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

  const sideInfo = (
    <SideInfo
      dataType="course"
      name={courseGQLData.department + ' ' + courseGQLData.courseNumber}
      title={courseGQLData.title}
      description={courseGQLData.description}
      tags={getCourseTags(courseGQLData)}
      course={courseGQLData}
      terms={courseGQLData.terms}
    />
  );

  return (
    <ResultPageContent sideInfo={sideInfo}>
      <ResultPageSection title="📊 Grade Distribution">
        <GradeDist dataType="course" data={courseGQLData} />
      </ResultPageSection>

      <ResultPageSection title="🌲 Prerequisite Tree">
        <PrereqTree key={courseGQLData.id} course={courseGQLData} />
      </ResultPageSection>

      <ResultPageSection title="🗓️ Schedule of Classes">
        <Schedule
          key={courseGQLData.id}
          courseID={courseGQLData.department + ' ' + courseGQLData.courseNumber}
          termsOffered={sortTerms(courseGQLData.terms)}
        />
      </ResultPageSection>

      <ResultPageSection title="💬 Reviews">
        <Review key={courseGQLData.id} course={courseGQLData} terms={sortTerms(courseGQLData.terms)} />
      </ResultPageSection>
    </ResultPageContent>
  );
};

export default CoursePage;
