import { FC } from 'react';
import './ResultPageContent.scss';
import Twemoji from 'react-twemoji';
import PrereqTree from '../PrereqTree/PrereqTree';
import { DataType, CourseGQLData, ProfessorGQLData, Section, NotPrereqTreeType } from '../../types/types';

interface ResultPageSectionProps {
  dataType: DataType;
  data: CourseGQLData | ProfessorGQLData;
  Component: Section['Component'];
}

const ResultPageSection: FC<ResultPageSectionProps> = ({ dataType, data, Component }) => {
  if (Component === PrereqTree) return <PrereqTree data={data as CourseGQLData} />;
  const ComponentToRender = Component as NotPrereqTreeType;
  return <ComponentToRender dataType={dataType} data={data} />;
};

interface ResultPageContentProps {
  dataType: DataType;
  data: CourseGQLData | ProfessorGQLData;
  sideInfo: React.ReactNode;
  mainSections: Section[];
}

const ResultPageContent: FC<ResultPageContentProps> = ({ dataType, data, sideInfo, mainSections }) => (
  <div className="content-wrapper search-result-page">
    <div className="side-info-wrapper">{sideInfo}</div>
    <span className="twemoji-wrapper">
      <Twemoji options={{ className: 'twemoji' }}>
        <div className="result-page-body">
          {mainSections.map(({ title, Component }) => (
            <div key={title} className="result-page-section">
              <h2>{title}</h2>
              <ResultPageSection dataType={dataType} data={data} Component={Component} />
            </div>
          ))}
        </div>
      </Twemoji>
    </span>
  </div>
);

export default ResultPageContent;
