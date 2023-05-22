import React, { FC, useState, useEffect } from 'react';
import axios, { AxiosResponse } from 'axios';
import SubReview from './SubReview';
import ReviewForm from '../ReviewForm/ReviewForm';
import './Review.scss';

import { selectReviews, setReviews, setFormStatus } from '../../store/slices/reviewSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { CourseGQLData, ProfessorGQLData, ReviewData, VoteColorsRequest, VoteColor } from '../../types/types';
import { Dropdown } from 'semantic-ui-react';

export interface ReviewProps {
    course?: CourseGQLData;
    professor?: ProfessorGQLData;
}

enum SortingOption {
    MOST_RECENT,
    TOP_REVIEWS,
    CONTROVERSIAL
}

const Review: FC<ReviewProps> = (props) => {
    const dispatch = useAppDispatch();
    const reviewData = useAppSelector(selectReviews);
    const [voteColors, setVoteColors] = useState([]);
    const openForm = useAppSelector(state => state.review.formOpen);
    const [sortingOption, setSortingOption] = useState<SortingOption>(SortingOption.MOST_RECENT);

    const getColors = async (vote: VoteColorsRequest) => {
        const res = await axios.patch('/api/reviews/getVoteColors', vote);
        return res.data;
    }

    const getReviews = async () => {
        interface paramsProps {
            courseID?: string;
            professorID?: string;
        }
        let params: paramsProps = {};
        if (props.course) params.courseID = props.course.id;
        if (props.professor) params.professorID = props.professor.ucinetid;
        axios.get(`/api/reviews`, {
            params: params
        })
            .then(async (res: AxiosResponse<ReviewData[]>) => {
                const data = res.data.filter((review) => review !== null);
                let reviewIDs = [];
                for(let i = 0;i<data.length;i++){
                    reviewIDs.push(data[i]._id);
                }
                const req = {
                    ids: reviewIDs as string[]
                }
                let colors = await getColors(req);
                setVoteColors(colors);
                dispatch(setReviews(data));
            });
    }

    const updateVoteColors = async () => {
        let reviewIDs = [];
        for(let i = 0;i<reviewData.length;i++){
            reviewIDs.push(reviewData[i]._id);
        }
        const req = {
            ids: reviewIDs as string[]
        }
        let colors = await getColors(req);
        setVoteColors(colors);
    }

    const getU = (id: string | undefined) => {
        let temp = voteColors as Object;
        let v = (temp[id as keyof typeof temp]) as unknown as number;
        if(v == 1){
            return {
                colors: [true, false]
            }
        }else if(v == -1){
            return {
                colors: [false, true]
            }
        }
        return {
            colors: [false, false]
        }
    }
    useEffect(() => {
        // prevent reviews from carrying over
        dispatch(setReviews([]));
        getReviews();
    }, [props.course?.id, props.professor?.ucinetid]);

    const openReviewForm = () => {
        dispatch(setFormStatus(true));
        document.body.style.overflow = 'hidden';
    }
    const closeForm = () => {
        dispatch(setFormStatus(false));
        document.body.style.overflow = 'visible';
    }

    if (!reviewData) {
        return <p>Loading reviews..</p>;
    } else {
        let sortedReviews = reviewData.slice(0); // clone since const
        switch (sortingOption) {
            case SortingOption.MOST_RECENT:
                sortedReviews.sort((a, b) => new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime());
                break;
            case SortingOption.TOP_REVIEWS: // the right side of || will fall back to most recent when score is equal
                sortedReviews.sort((a, b) => b.score - a.score || new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime());
                break;
            case SortingOption.CONTROVERSIAL:
                sortedReviews.sort((a, b) => a.score - b.score || new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime());
                break;
        }

        return (
            <>
                <Dropdown
                    placeholder='Chart Type'
                    scrolling
                    selection
                    options={[{ text: 'Most Recent', value: SortingOption.MOST_RECENT },
                        { text: 'Top Reviews', value: SortingOption.TOP_REVIEWS },
                        { text: 'Controversial', value: SortingOption.CONTROVERSIAL }]}
                    value={sortingOption}
                    onChange={(e, s) => setSortingOption(s.value as SortingOption)}
                />
                <div className='reviews'>
                    {sortedReviews.map((review, i) => <SubReview review={review} key={review._id} course={props.course} professor={props.professor} colors={getU(review._id) as VoteColor} colorUpdater={updateVoteColors}/>)}
                    <button type='button' className='add-review-btn' onClick={openReviewForm}>+ Add Review</button>
                </div>
                <ReviewForm closeForm={closeForm} {...props} />
            </>
        )
    }
}

export default Review;