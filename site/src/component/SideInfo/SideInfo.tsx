import React, { FC, useEffect, useState } from 'react';
import './SideInfo.scss';
import Badge from 'react-bootstrap/Badge';
import DropdownButton from 'react-bootstrap/DropdownButton';
import Dropdown from 'react-bootstrap/Dropdown';
import Button from 'react-bootstrap/Button';
import { Link } from 'react-router-dom';

import { CourseData, ProfessorData, SearchType, ReviewData } from '../../types/types';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { toggleFormStatus } from '../../store/slices/reviewSlice';

interface FeaturedInfoData {
    searchType: SearchType;
    featureType: 'Highest' | 'Lowest';
    averageReviews: { [key: string]: AverageReview };
    reviewKey: string;
}

const FeaturedInfo: FC<FeaturedInfoData> = ({ searchType, featureType, averageReviews, reviewKey }) => {
    return <div className='side-info-feature'>
        <div className='side-info-feature-title'>
            {featureType} Rated {searchType == 'course' ? 'Instructor' : 'Course'}
        </div>
        <div className='side-info-feature-based'>
            Based on {averageReviews[reviewKey].count} reviews
        </div>
        <div className='side-info-feature-name'>
            <Link to={{ pathname: `/${searchType == 'course' ? 'professor' : 'course'}/${reviewKey}` }}>
                {reviewKey}
            </Link>
        </div>
        <div className='side-info-feature-stats'>
            <div className='side-info-feature-stat'>
                <div className='side-info-feature-stat-label'>
                    Rating
                </div>
                <div className='side-info-feature-stat-value'>
                    {(averageReviews[reviewKey].rating / averageReviews[reviewKey].count).toFixed(2)}
                    <span className='side-info-denominator'>
                        / 5.0
                    </span>
                </div>
            </div>
            <div className='side-info-feature-stat'>
                <div className='side-info-feature-stat-label'>
                    Difficulty
                </div>
                <div className='side-info-feature-stat-value'>
                    {(averageReviews[reviewKey].difficulty / averageReviews[reviewKey].count).toFixed(2)}
                    <span className='side-info-denominator'>
                        / 5.0
                    </span>
                </div>
            </div>
        </div>
    </div>
}

interface SideInfoProps {
    searchType: SearchType;
    name: string;
    title: string;
    school: string;
    description: string;
    tags: string[];
    course?: CourseData;
    professor?: ProfessorData;
}

interface AverageReview {
    count: number;
    rating: number;
    difficulty: number;
}

const SideInfo: FC<SideInfoProps> = (props) => {
    const dispatch = useAppDispatch();
    const allToken = 'All ' + (props.searchType == 'course' ? 'Instructors' : 'Courses');
    const reviews = useAppSelector(state => state.review.reviews);
    const [averageReviews, setAverageReviews] = useState<{ [key: string]: AverageReview }>({});
    const [highestReview, setHighestReview] = useState('');
    const [lowestReview, setLowestReview] = useState('');
    const [selectedReview, setSelectedReview] = useState('');

    useEffect(() => {
        let newAverageReviews: { [key: string]: AverageReview } = {};
        let allReviews = {
            count: 0,
            rating: 0,
            difficulty: 0
        };

        reviews.forEach(review => {
            let key = '';
            // determine the key to group reviews by
            if (props.searchType == 'course') {
                key = review.professorID;
            }
            else if (props.searchType == 'professor') {
                key = review.courseID;
            }

            // add review entry
            if (!newAverageReviews.hasOwnProperty(key)) {
                newAverageReviews[key] = {
                    count: 0,
                    rating: 0,
                    difficulty: 0
                }
            }
            newAverageReviews[key].count += 1;
            newAverageReviews[key].rating += review.rating;
            newAverageReviews[key].difficulty += review.difficulty;
            allReviews.count += 1;
            allReviews.rating += review.rating;
            allReviews.difficulty += review.difficulty;
        })

        // find highest and lowest reviews
        let sortedKeys = Object.keys(newAverageReviews);
        sortedKeys.sort((a, b) => (newAverageReviews[a].rating / newAverageReviews[a].count - newAverageReviews[b].rating / newAverageReviews[b].count));

        // set the all token to all reviews
        newAverageReviews[allToken] = allReviews;

        // set reviews to state
        setAverageReviews(newAverageReviews);
        setSelectedReview(allToken);
        if (sortedKeys.length > 0) {
            setHighestReview(sortedKeys[sortedKeys.length - 1]);
            setLowestReview(sortedKeys[0]);
        }
    }, [reviews])

    return (
        <div className='side-info'>
            <div className='side-info-data'>
                <h1>
                    {props.name}
                </h1>
                <h2>
                    {props.title}
                </h2>
                <h3>
                    {props.school}
                </h3>
                <h4>
                    {props.description}
                </h4>
                <div>
                    {
                        props.tags.map(tag => <Badge pill className='p-3 mr-3' variant='info'>
                            {tag}
                        </Badge>)
                    }
                </div>
            </div>

            <div className='side-info-ratings'>
                <div className='side-info-buttons'>
                    <DropdownButton title={selectedReview} variant='secondary' onSelect={(e) => {
                        setSelectedReview(e as string);
                    }}>
                        {
                            Object.keys(averageReviews).map((key, index) => <Dropdown.Item eventKey={key}>
                                {key}
                            </Dropdown.Item>)
                        }
                    </DropdownButton>
                    <Button variant="primary" onClick={() => {
                        dispatch(toggleFormStatus());
                    }}>Rate {props.searchType}</Button>
                </div>
                {selectedReview && <div className='side-info-selected-rating'>
                    <div className='side-info-stat'>
                        <div className='side-info-stat-label'>
                            Rating
                        </div>
                        <div className='side-info-stat-value'>
                            {(averageReviews[selectedReview].rating / averageReviews[selectedReview].count).toFixed(2)}
                            <span className='side-info-denominator'>
                                / 5.0
                            </span>
                        </div>
                    </div>

                    <div className='side-info-stat'>
                        <div className='side-info-stat-label'>
                            Difficulty
                        </div>
                        <div className='side-info-stat-value'>
                            {(averageReviews[selectedReview].difficulty / averageReviews[selectedReview].count).toFixed(2)}
                            <span className='side-info-denominator'>
                                / 5.0
                            </span>
                        </div>
                    </div>

                    <div className='side-info-stat'>
                        <div className='side-info-stat-label'>
                            Responses
                        </div>
                        <div className='side-info-stat-value'>
                            {averageReviews[selectedReview].count}
                        </div>
                    </div>
                </div>
                }
            </div>

            <div className='side-info-featured'>
                {highestReview && <FeaturedInfo searchType={props.searchType} featureType='Highest'
                    averageReviews={averageReviews} reviewKey={highestReview} />}
                {lowestReview && <FeaturedInfo searchType={props.searchType} featureType='Lowest'
                    averageReviews={averageReviews} reviewKey={lowestReview} />}
            </div>
        </div>
    )
}

export default SideInfo;