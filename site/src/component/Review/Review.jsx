import React, { useState, useEffect } from 'react';
import axios from 'axios';
import SubReview from "./SubReview";
import ReviewForm from '../ReviewForm/ReviewForm';
import './Review.scss'

export default function Review(props) {
    const [reviewData, setReviewData] = useState(null);
    const [openForm, setOpenForm] = useState(false);

    const [addedReview, setAddedReview] = useState(false);

    const getReviews = async () => {
        axios.get('/reviews').then((res) => {
            const data = res.data.data.allReviews.data.filter((review) => review !== null && review.courseID === props.id)
            console.log('hello');
            setReviewData(data);
            console.log(reviewData);
        });
        
    }

    useEffect(() => {
        getReviews();
    }, [])

    useEffect(() => {
        if (addedReview) {
            window.location.reload();
        }
    }, [addedReview])

    const openReviewForm = () => {
        setOpenForm(true);
        document.body.style.overflow = "hidden";
    }
    const closeForm = () => {
        setOpenForm(false);
        document.body.style.overflow = "visible";
    }

    // TODO: connect with backend api and query database

    if (!reviewData) {
        return <p>Loading reviews..</p>;
    } else {
        return (
            <>
                <div className="reviews">
                    {reviewData.map((review, i) => {
                        if (review !== null) return (<SubReview review={review} key={i}/>)
                    })}
                    <button type="button" className="add-review-btn" onClick={openReviewForm}>+ Add Review</button>
                </div>
                {openForm ? (
                    <>
                    <div className="blur-bg" onClick={closeForm}/>
                    <ReviewForm {...props} setAddedReview={setAddedReview}/>
                    </>
                ) : null}
            </>
        )

    }
}