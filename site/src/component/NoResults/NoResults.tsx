import { FC } from 'react';
import './NoResults.scss';
import noResultsImg from '../../asset/no-results-crop.webp';

interface NoResultsProps {
  notSearching: boolean;
  notSearchingText: string;
}
const NoResults: FC<NoResultsProps> = ({ notSearching, notSearchingText }) => {
  return (
    <div className="no-results">
      <img src={noResultsImg} alt="No results found" />
      {notSearching ? notSearchingText : "Sorry, we couldn't find any results for that search!"}
    </div>
  );
};

export default NoResults;
