<?php
App::uses('AppModel', 'Model');
/**
 * Comment Model
 *
 * @property Post $Post
 */
class Comment extends AppModel {
/**
 * Display field
 *
 * @var string
 */
    public $displayField = 'comment';

    //The Associations below have been created with all possible keys, those that are not needed can be removed

/**
 * belongsTo associations
 *
 * @var array
 */
    public $belongsTo = array(
        'Post' => array(
            'className' => 'Post',
            'foreignKey' => 'post_id',
            'conditions' => '',
            'fields' => '',
            'order' => ''
        )
    );
}
