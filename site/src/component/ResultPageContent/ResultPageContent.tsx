import { FC } from 'react';
import Twemoji from 'react-twemoji';
import './ResultPageContent.scss';

interface ResultPageSectionProps {
  title: string;
  children: React.ReactNode;
}

const ResultPageSection: FC<ResultPageSectionProps> = ({ title, children }) => {
  return (
    <div className="result-page-section">
      <h2>{title}</h2>
      {children}
    </div>
  );
};

type ResultPageContentProps = {
  sideInfo: React.ReactNode;
  mainSections: {
    title: string;
    Component: JSX.Element;
  }[];
};

const ResultPageContent: FC<ResultPageContentProps> = ({ sideInfo, mainSections }) => {
  return (
    <div className="content-wrapper search-result-page">
      <div className="side-info-wrapper">{sideInfo}</div>
      <span className="twemoji-wrapper">
        <Twemoji options={{ className: 'twemoji' }}>
          <div className="result-page-body">
            {mainSections.map((section, index) => (
              <ResultPageSection key={index} title={section.title}>
                {section.Component}
              </ResultPageSection>
            ))}
          </div>
        </Twemoji>
      </span>
    </div>
  );
};

export default ResultPageContent;
