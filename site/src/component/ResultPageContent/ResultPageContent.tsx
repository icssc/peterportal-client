import { FC } from 'react';
import Twemoji from 'react-twemoji';
import './ResultPageContent.scss';

interface ResultPageSectionProps {
  title: string;
  children: React.ReactNode;
}

export const ResultPageSection: FC<ResultPageSectionProps> = ({ title, children }) => {
  return (
    <div className="result-page-section">
      <h2>{title}</h2>
      {children}
    </div>
  );
};

interface ResultPageContentProps {
  sideInfo: React.ReactNode;
  mainContent: React.ReactNode;
}

export const ResultPageContent: FC<ResultPageContentProps> = ({ sideInfo, mainContent }) => {
  return (
    <div className="content-wrapper search-result-page">
      <div className="side-info-wrapper">{sideInfo}</div>
      <span className="twemoji-wrapper">
        <Twemoji options={{ className: 'twemoji' }}>
          <div className="result-page-body">{mainContent}</div>
        </Twemoji>
      </span>
    </div>
  );
};
