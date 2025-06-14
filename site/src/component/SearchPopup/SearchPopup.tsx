import { FC } from 'react';
import { Link } from 'react-router-dom';
import Button from 'react-bootstrap/Button';
import Carousel from 'react-multi-carousel';
import './SearchPopup.scss';

import GradeDist from '../GradeDist/GradeDist';
import searching from '../../asset/searching.webp';
import { CourseGQLData, ProfessorGQLData, DataType, ScoreData } from '../../types/types';

interface SearchPopupCarouselProps {
  dataType: DataType;
  scores: ScoreData[];
}

const SearchPopupCarousel: FC<SearchPopupCarouselProps> = ({ dataType, scores }) => {
  const responsive = {
    desktop: {
      breakpoint: { max: 3000, min: 1024 },
      items: 3,
      partialVisibilityGutter: 60,
    },
    tablet: {
      breakpoint: { max: 1024, min: 464 },
      items: 2,
      partialVisibilityGutter: 50,
    },
    mobile: {
      breakpoint: { max: 464, min: 0 },
      items: 1,
      partialVisibilityGutter: 30,
    },
  };

  const getCarouselScore = (rating: number) => {
    return rating === -1 ? '?' : Number.isInteger(rating) ? rating : rating.toFixed(2);
  };

  return (
    <Carousel responsive={responsive} renderButtonGroupOutside>
      {scores.map((datum, i) => (
        <div key={`search-popup-carousel-${i}`} className="search-popup-carousel search-popup-block">
          <div>
            <span className="search-popup-carousel-score">{getCarouselScore(datum.avgRating)}</span>
            <span className="search-popup-carousel-max-score">/ 5.0</span>
          </div>
          <Link to={`/${dataType === 'course' ? 'professor' : 'course'}/${datum.id}`}>
            <span className="search-popup-professor-name" title={datum.name}>
              {datum.name.split(' ').map((part, i) => (
                <span key={i} className={part.length > 13 ? 'ellipsis' : ''}>
                  {part}
                </span>
              ))}
            </span>
          </Link>
        </div>
      ))}
    </Carousel>
  );
};

export const SearchPopupPlaceholder: FC<Pick<SearchPopupProps, 'dataType'>> = (props) => {
  return (
    <div className="side-panel search-popup">
      <div className="search-popup-missing">
        <img style={{ width: '80%' }} src={searching} alt="searching" />
        <p>Click on a {props.dataType} card to view more information!</p>
      </div>
    </div>
  );
};

interface InfoData {
  title: string;
  content: string;
}

interface SearchPopupProps {
  dataType: DataType;
  data: CourseGQLData | ProfessorGQLData;
  id: string;
  name: string;
  title: string;
  infos: InfoData[];
  scores: ScoreData[];
}

export const SearchPopup: FC<SearchPopupProps> = (props) => {
  return (
    <div className="side-panel search-popup">
      <div className="search-popup-header">
        <h2 className="search-popup-id">
          {props.name}
          <Link to={`/${props.dataType}/${props.id}`}>
            <Button type="button" className="search-popup-more btn btn-outline-primary">
              More Information
            </Button>
          </Link>
        </h2>
        <h5 className="search-popup-title">{props.title}</h5>
      </div>
      <div>
        <div className="search-popup-infos">
          {props.infos.map((info, i) => (
            <div className="search-popup-info search-popup-block" key={`search-popup-info-${i}`}>
              <h3>{info.title}</h3>
              <p>{info.content || `No ${info.title}`}</p>
            </div>
          ))}
        </div>

        <h2 className="search-popup-label">Grade Distribution</h2>
        <div className="search-popup-block">
          <GradeDist dataType={props.dataType} data={props.data} minify={true} />
        </div>

        <h2 className="search-popup-label">
          {props.dataType === 'course' ? 'Current Instructors' : 'Previously Taught'}
        </h2>
        <div>{props.scores.length > 0 ? <SearchPopupCarousel {...props} /> : 'No Instructors Found'}</div>
      </div>
    </div>
  );
};
