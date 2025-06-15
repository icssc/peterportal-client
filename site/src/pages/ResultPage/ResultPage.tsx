import { FC, useState, useEffect } from 'react';
import './ResultPage.scss';
import { useParams } from 'react-router-dom';
import Twemoji from 'react-twemoji';

import GradeDist from '../../component/GradeDist/GradeDist';
import PrereqTree from '../../component/PrereqTree/PrereqTree';
import Schedule from '../../component/Schedule/Schedule';
import Review from '../../component/Review/Review';
import SideInfo from '../../component/SideInfo/SideInfo';
import Error from '../../component/Error/Error';
import LoadingSpinner from '../../component/LoadingSpinner/LoadingSpinner';

import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { setCourse, setProfessor } from '../../store/slices/popupSlice';
import { getCourseTags, searchAPIResult, getSentenceCase, getCourseId } from '../../helpers/util';
import { CourseGQLData, ProfessorGQLData, GQLData, GQLDataType } from '../../types/types';

type PrereqTreeType = React.ComponentType<{ data: CourseGQLData }>;
type NotPrereqTreeType = React.ComponentType<{ data: GQLData }>;
type ComponentType = PrereqTreeType | NotPrereqTreeType;

interface ResultPageSectionComponentProps {
  data: GQLData;
  Component: ComponentType;
}

const ResultPageSectionComponent: FC<ResultPageSectionComponentProps> = ({ data, Component }) => {
  if (Component === PrereqTree) return <PrereqTree data={data as CourseGQLData} />;
  const ComponentToRender = Component as NotPrereqTreeType;
  return <ComponentToRender data={data} />;
};

interface DataProp {
  data: GQLData;
}

const SideInfoWrapper: FC<DataProp> = ({ data }) => {
  if (data.type === 'course') {
    const course = data as CourseGQLData;
    return (
      <SideInfo
        data={course}
        name={getCourseId(course)}
        title={course.title}
        description={course.description}
        tags={getCourseTags(course)}
        terms={course.terms}
      />
    );
  }
  const professor = data as ProfessorGQLData;
  return (
    <SideInfo
      data={professor}
      name={professor.name}
      title={professor.title}
      description={professor.department}
      tags={[professor.ucinetid, ...professor.shortenedNames]}
    />
  );
};

const MainContent: FC<DataProp> = ({ data }) => {
  const mainSections = [
    { title: '📊 Grade Distribution', Component: GradeDist },
    { title: '🌲 Prerequisite Tree', Component: PrereqTree },
    { title: '🗓️ Schedule of Classes', Component: Schedule },
    { title: '💬 Reviews', Component: Review },
  ].filter((section) => data.type === 'course' || section.Component !== PrereqTree);

  return (
    <div className="result-page-body">
      {mainSections.map(({ title, Component }) => (
        <div key={title} className="result-page-section">
          <h2>{title}</h2>
          <ResultPageSectionComponent data={data} Component={Component} />
        </div>
      ))}
    </div>
  );
};

interface ResultPageProps {
  dataType: GQLDataType;
}

const ResultPage: FC<ResultPageProps> = ({ dataType }) => {
  const { id } = useParams<{ id: string }>();
  const dispatch = useAppDispatch();
  const { course, professor } = useAppSelector((state) => state.popup);
  const data = dataType === 'course' ? course : professor;
  const [error, setError] = useState('');

  useEffect(() => {
    if (!id) return;

    searchAPIResult(dataType, id).then((result) => {
      if (!result) {
        setError(`${getSentenceCase(dataType)} ${id} does not exist!`);
        return;
      }

      result.type = dataType;

      const resultAsCourse = result as CourseGQLData;
      const resultAsProfessor = result as ProfessorGQLData;

      dispatch(dataType === 'course' ? setCourse(resultAsCourse) : setProfessor(resultAsProfessor));
      setError('');

      const dataName = dataType === 'course' ? getCourseId(resultAsCourse) : resultAsProfessor.name;
      document.title = `${dataName} | PeterPortal`;
    });
  }, [dataType, id, dispatch]);

  if (error) {
    return <Error message={error} />;
  }

  if (!data) {
    return <LoadingSpinner />;
  }

  return (
    <div className="content-wrapper result-page">
      <div className="side-info-wrapper">
        <SideInfoWrapper data={data} />
      </div>
      <span className="twemoji-wrapper">
        <Twemoji options={{ className: 'twemoji' }}>
          <MainContent data={data} />
        </Twemoji>
      </span>
    </div>
  );
};

export default ResultPage;
