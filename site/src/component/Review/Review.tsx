import React, { FC, useState, useEffect } from 'react';
import axios, { AxiosResponse } from 'axios';
import SubReview from './SubReview';
import ReviewForm from '../ReviewForm/ReviewForm';
import './Review.scss';

import { selectReviews, setReviews, setFormStatus } from '../../store/slices/reviewSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { CourseGQLData, ProfessorGQLData, ReviewData, VoteColorsRequest, VoteColor } from '../../types/types';
import { reviewSlice } from 'src/store/slices/uiSlice';

export interface ReviewProps {
    course?: CourseGQLData;
    professor?: ProfessorGQLData;
}

const Review: FC<ReviewProps> = (props) => {
    const dispatch = useAppDispatch();
    const reviewData = useAppSelector(selectReviews);
    const [voteColors, setVoteColors] = useState([]);
    const openForm = useAppSelector(state => state.review.formOpen);

    const getColors = async (vote: VoteColorsRequest) => {
        const res = await axios.patch('/reviews/getVoteColors', vote);
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
        axios.get(`/reviews`, {
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

    useEffect(() => {
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
        return (
            <>
                <div className='reviews'>
                    {reviewData.map((review, i) => {
                        if (review !== null) return (<SubReview review={review} key={i} course={props.course} professor={props.professor} colors={{colors: voteColors[i]} as VoteColor} colorUpdater={updateVoteColors}/>)
                    })}
                    <button type='button' className='add-review-btn' onClick={openReviewForm}>+ Add Review</button>
                </div>
                {openForm ? (
                    <>
                        <div className='blur-bg' onClick={closeForm} />
                        <ReviewForm closeForm={closeForm} {...props} />
                    </>
                ) : null}
            </>
        )
    }
}

export default Review;