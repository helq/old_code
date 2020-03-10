// compile with: gcc -std=c99 -Wall -g islands.c -lpthread -o islands
//
// parent thread: glud
// children threads: people (kids or adults)
//
// licence WTFPL
//
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define bool int
#define true 1
#define false 0

// defining the number of people in the islands
#define KIDS_OAHU       3
#define ADULTS_OAHU     5
#define TOTAL_OAHU      (KIDS_OAHU + ADULTS_OAHU)
#define KIDS_MOLOKAI    0
#define ADULTS_MOLOKAI  6
#define TOTAL_MOLOKAI   (KIDS_MOLOKAI + ADULTS_MOLOKAI)
#define TOTAL_PEOPLE    (TOTAL_OAHU + TOTAL_MOLOKAI)

// islands of Hawaii
#define OAHU        0x15
#define MOLOKAI     0x62
#define BOAT        0x25

// the boat and its resources
struct boat {
    int name;
    void *parked;

    // the boat know the islands but it shouldn't parked in some island if not
    // is not throwing.
    struct oahu *oahu;
    struct molokai *molokai;

    bool pilot;
    bool copilot;

    bool rowing;

    pthread_mutex_t mutex_boat;
    pthread_cond_t  cond_boat; // used for syncronize the get off of the boat
    pthread_cond_t  cond_boat2; // used for syncronize the get off of the boat
};

#define boat_parked(island) ( ((struct basic_island *) island)->boat != NULL)
#define boat_occupied(boat)  ((boat)->pilot == true)
#define boat_with_copilot(boat) ((boat)->copilot == true)

#define get_on_board_as_pilot(boat) (boat)->pilot = true
#define get_on_board_as_copilot(b) {\
    (b)->copilot = true; \
    wplace->place = ((struct basic_island *)wplace->place)->boat; }

#define get_off_board_as_pilot(boat) (boat)->pilot = false
#define get_off_board_as_copilot(boat) (boat)->copilot = false

// scheme of an island structure
struct basic_island {
    int name;
    struct boat *boat;
};

#define ubication(wplace) *((int *) wplace->place)

#define lifting_the_anchors(wplace) {\
    struct basic_island *island = wplace->place; \
    wplace->place = island->boat; \
    ( (struct boat *)wplace->place )->parked = NULL; \
    island->boat = NULL; }

#define arrived_(island, wplace) {\
    struct boat *b = wplace->place; \
    b->parked = b->island; \
    wplace->place = b->island; \
    ((struct basic_island *) wplace->place)->boat = b; }

// island of oahu
struct oahu {
    int name;
    struct boat *boat;

    int population;
    pthread_mutex_t mutex_counter; // for count the population

    // for select only two kids from possibly more than two kids
    int selected_kids;
    pthread_mutex_t mutex_oahu;

    // when kids set this condition then it's possible get on the boat
    pthread_cond_t wait_signal_kids;
    bool boat_ready;

    // the selected kid wait for her copilot
    pthread_cond_t waiting_copilot;

    // one selected kid wait the boat and the second selected kid that come
    // from Molokai
    pthread_cond_t waiting_second_kid;

};

// island of molokai
struct molokai {
    int name;
    struct boat *boat;
    pthread_mutex_t mutex_molokai;
    pthread_cond_t waiting_boat; // selected kid wait the return of the boat
    bool first_gone; // know if is the first selected kid who come back to Oahu
};

// this is for each person, with this glud (parent thread) can see the state
// (the place) of his children (each person)
struct wrapped_place {
    void *place;
    struct glud_communication *glud_com;
};

// for synchronization between threads children (the people) and the parent
// thread (glud)
struct glud_communication {
    bool *glud_say_finished;
    bool *finished;

    pthread_mutex_t *mutex_glud;
    pthread_cond_t *people_wait_glud;
    pthread_cond_t *glud_wait_kids;
};

// the trow made by each kid (no selected kids) and adult in Oahu
void trow_to_molokai(struct wrapped_place *wplace, char *kid_or_adult)
{
    // actual place: Oahu
    {
        struct oahu *oahu = wplace->place;

        // add herself to the population
        pthread_mutex_lock( &(oahu->mutex_counter) );
          (oahu->population) ++;
          printf("population Oahu: %2d\n", oahu->population);
        pthread_mutex_unlock( &(oahu->mutex_counter) );

        pthread_mutex_lock( & (oahu->mutex_oahu) );
          // wait while the boat is NOT ready
          if(!oahu->boat_ready)
              do  pthread_cond_wait( &(oahu->wait_signal_kids)
                                   , &(oahu->mutex_oahu));
              while(!boat_parked(oahu) || boat_occupied(oahu->boat));

          get_on_board_as_pilot(oahu->boat);

          // the boat is now occupied, then is NOT ready
          oahu->boat_ready = false;

          lifting_the_anchors(wplace);

        pthread_mutex_unlock( & (oahu->mutex_oahu) );

        // delete herself to the population
        pthread_mutex_lock( &(oahu->mutex_counter) );
          (oahu->population) --;
          printf("population Oahu: %2d\n", oahu->population);
        pthread_mutex_unlock( &(oahu->mutex_counter) );
    }

    // actual place: the boat
    {
        struct boat *boat = wplace->place;

        // rowing, rowing, rowing
        printf("%s rowing to molokai\n", kid_or_adult);

        arrived_(molokai, wplace);

        printf("%s arrive molokai\n", kid_or_adult);
        get_off_board_as_pilot(boat);
    }

    // actual place: Molokai
    {
        struct molokai *molokai = wplace->place;

        // one selected kid is waiting for the boat, the person inform about
        // the boat that is ready in Molokai
        pthread_mutex_lock( &(molokai->mutex_molokai) );
          pthread_cond_signal( &(molokai->waiting_boat) );
        pthread_mutex_unlock( &(molokai->mutex_molokai) );
    }
}

// kids behavior
void *kid(void *wrapped_place)
{
    // now I, one kid, appear in somewhere
    struct wrapped_place *wplace = (struct wrapped_place *) wrapped_place;

    // where am I?
    switch ( ubication(wplace) ) {
      case OAHU:
        // kid in Oahu
        printf("kid in Oahu\n");

        // only two kids are selected, then they need know who they are
        bool i_am_selected = false;
        {
            struct oahu *oahu = wplace->place;

            pthread_mutex_lock( & (oahu->mutex_oahu) );
            // if not all kids are selected (two kids will selected), then
            // she is selected
            if(oahu->selected_kids < 2) {
                oahu->selected_kids ++;
                i_am_selected = true;
            }
            pthread_mutex_unlock( & (oahu->mutex_oahu) );
        }

        // if the kid are not selected then she do exactly that the adults do
        if(!i_am_selected) {
            trow_to_molokai(wplace, "kid");

        // the kid was selected, then she is one of the leaders of the operation
        } else {
            printf("kid selected\n");

            // do the next routine while glud don't say that every is finished
            // (note: while `break`s in the middle)
            while(true) {
                // for know if is the pilot or the copilot
                bool i_am_pilot = false;
                int posible_population_oahu;

                // kid in Oahu
                {
                    struct oahu *oahu = wplace->place;

                    // count the population while on Oahu
                    posible_population_oahu = oahu->population;

                    pthread_mutex_lock( &(oahu->mutex_oahu) );
                    // if the boat is not occupied, then
                    if(!boat_occupied(oahu->boat)) {
                        // the first on the boat do:

                        get_on_board_as_pilot(oahu->boat);
                        i_am_pilot = true;
                        printf("kid* as pilot (in oahu)\n");

                        // wait while the copilot get on the boat
                        do pthread_cond_wait( &(oahu->waiting_copilot)
                                            , &(oahu->mutex_oahu));
                        while(!boat_with_copilot(oahu->boat));
                        lifting_the_anchors(wplace);

                    // if someone is on the boat, then
                    } else {
                        // the second on the boat (is the copilot):
                        get_on_board_as_copilot(oahu->boat);
                        printf("kid* as copilot (in oahu)\n");
                        // say to the pilot that the copilot is on board
                        pthread_cond_signal( &(oahu->waiting_copilot) );
                    }
                    pthread_mutex_unlock( &(oahu->mutex_oahu) );
                }

                // kid in the boat
                {
                    struct boat *boat = wplace->place;

                    if(i_am_pilot) {
                        // the pilot do

                        pthread_mutex_lock( &(boat->mutex_boat) );
                        // the pilot row to Molokai
                        boat->rowing = true;
                        printf("kid* rowing to molokai\n");

                        // if the copilot is waiting for the pilot then
                        // inform she that the boat is now rowing
                        pthread_cond_signal( &(boat->cond_boat2) );

                        // wait while the copilot arrive to the island
                        do pthread_cond_wait( &(boat->cond_boat)
                                            , &(boat->mutex_boat) );
                        while (boat_with_copilot(boat));

                        // the boat arrive
                        arrived_(molokai, wplace);

                        // now the boat is not rowing
                        boat->rowing = false;

                        // get off the boat
                        get_off_board_as_pilot(boat);

                        printf("kid* arrive Molokai\n");

                        // say to the copilot that the boat is now parked
                        pthread_cond_signal( &(boat->cond_boat2) );

                        pthread_mutex_unlock( &(boat->mutex_boat) );

                    } else {
                        // the copilot do

                        pthread_mutex_lock( &(boat->mutex_boat) );
                        // if the boat is not rowing wait for the pilot inform
                        // about that
                        while( !(boat->rowing) )
                            pthread_cond_wait( &(boat->cond_boat2)
                                             , &(boat->mutex_boat) );

                        // now, the pilot is rowing to Molokai, then I ride to
                        // Molokai
                        printf("kid* ride to Molokai\n");

                        // the boat arrive Molokai
                        wplace->place = boat->molokai;

                        // the kid get off the boat
                        get_off_board_as_copilot(boat);

                        printf("kid* arrive Molokai\n");
                        // say to the pilot that the boat arrived
                        pthread_cond_signal( &(boat->cond_boat) );

                        // wait for the pilot, who is park the boat.
                        pthread_cond_wait( &(boat->cond_boat2)
                                         , &(boat->mutex_boat) );

                        pthread_mutex_unlock( &(boat->mutex_boat) );
                    }
                }

                bool the_end = false;
                // if the kid remaind that was no one on the island then she
                // communicate with Glud
                if(posible_population_oahu == 0) {

                    pthread_mutex_lock( wplace->glud_com->mutex_glud );
                      // if Glud say that everything is ok, then is the end
                      if(*(wplace->glud_com->glud_say_finished))
                          the_end = true;

                      // if Glud not said that everything is ok, then ask him
                      else {
                          // for me is everything ok,
                          *(wplace->glud_com->finished) = true;
                          printf("questioning to glud\n");

                          // , and for you Glud, is everything okay?
                          pthread_cond_signal( wplace->glud_com->glud_wait_kids );

                          // wait for the answer of Glud
                          pthread_cond_wait( wplace->glud_com->people_wait_glud
                                           , wplace->glud_com->mutex_glud );

                          // if Glud say that everything is ok, then is the end
                          if(*(wplace->glud_com->glud_say_finished))
                              the_end = true;
                      }
                    pthread_mutex_unlock( wplace->glud_com->mutex_glud );

                    // is it the end?
                    if(the_end)
                        break;
                }

                bool first_kid = true;
                // kid in Molokai
                {
                    struct molokai *molokai = wplace->place;

                    pthread_mutex_lock( &(molokai->mutex_molokai) );
                      // if the first kid is gone now then
                      if(! molokai->first_gone)
                          // I'm the first kid to return to Oahu, then
                          molokai->first_gone = true;

                      else {
                          // I'm not the first kid gone to Oahu
                          first_kid = false;
                          // wait while the boat is not in Molokai
                          while (!boat_parked(molokai))
                              pthread_cond_wait( &(molokai->waiting_boat)
                                               , &(molokai->mutex_molokai) );

                          // now, nobody will be in the island then reset the
                          // variable
                          molokai->first_gone = false;
                      }

                      // get on boat and return to Oahu
                      get_on_board_as_pilot(molokai->boat);
                      lifting_the_anchors(wplace);

                    pthread_mutex_unlock( &(molokai->mutex_molokai) );
                }

                // kid on boat
                {
                    struct boat *boat = wplace->place;

                    printf("kid* rowing to Oahu\n");

                    arrived_(oahu, wplace);

                    printf("kid* arrive Oahu\n");

                    get_off_board_as_pilot(boat);
                }

                // kid in Oahu
                if(first_kid)
                {
                    // the first kid to return to Oahu do:
                    struct oahu *oahu = wplace->place;

                    pthread_mutex_lock( &(oahu->mutex_oahu) );

                      // the kid inform to the people of the Oahu that the boat
                      // is ready
                      oahu->boat_ready = true;
                      pthread_cond_signal( &(oahu->wait_signal_kids) );

                      // the kid wait the second kid that will return from
                      // Molokai
                      pthread_cond_wait( &(oahu->waiting_second_kid)
                                       , &(oahu->mutex_oahu));

                    pthread_mutex_unlock( &(oahu->mutex_oahu) );
                }
                else
                {
                    struct oahu *oahu = wplace->place;

                    // when de second kid return to Oahu inform to the first
                    // kid that she is now in the island
                    pthread_mutex_lock( &(oahu->mutex_oahu) );
                      pthread_cond_signal( &(oahu->waiting_second_kid) );
                    pthread_mutex_unlock( &(oahu->mutex_oahu) );
                }

            }
        }

        break;
      case MOLOKAI:
        printf("kid in molokai\n");
        break;
      default:
        printf("something went wrong!");
        break;
    }

    pthread_mutex_lock( wplace->glud_com->mutex_glud );
    // wait for the signal of Glud to die
    while(! *(wplace->glud_com->glud_say_finished))
        pthread_cond_wait( wplace->glud_com->people_wait_glud
                         , wplace->glud_com->mutex_glud );
    pthread_mutex_unlock( wplace->glud_com->mutex_glud );

    // well, kid die
    puts("kid say bye");

    return NULL;
}

// adults behavior
void *adult(void *wrapped_place)
{
    // now I, one adult, appear in somewhere
    struct wrapped_place *wplace = (struct wrapped_place *) wrapped_place;

    // where am I?
    switch ( ubication(wplace) ) {
      case OAHU:
        printf("adult in oahu\n");
        trow_to_molokai(wplace, "adult");
        break;
      case MOLOKAI:
        printf("adult in molokai\n");
        // nothing to do
        break;
      default:
        printf("something went wrong!");
        break;
    }

    pthread_mutex_lock( wplace->glud_com->mutex_glud );
    // wait for the signal of Glud to die
    while(! *(wplace->glud_com->glud_say_finished))
        pthread_cond_wait( wplace->glud_com->people_wait_glud
                         , wplace->glud_com->mutex_glud );
    pthread_mutex_unlock( wplace->glud_com->mutex_glud );

    // well, adult die
    puts("adult say bye");

    return NULL;
}

// parent thread (glud), who create the people and send the signal to die
int main()
{
    printf("*** simulation initialized ***\n");

    // one thread for each person
    pthread_t people[TOTAL_PEOPLE];
    // state of each person
    struct wrapped_place people_state[TOTAL_PEOPLE];

    // initializing the islands and the boat
    struct oahu    oahu    = { .name = OAHU

                             , .population = 0
                             , .mutex_counter = PTHREAD_MUTEX_INITIALIZER

                             , .selected_kids = 0
                             , .mutex_oahu = PTHREAD_MUTEX_INITIALIZER
                             , .wait_signal_kids = PTHREAD_COND_INITIALIZER
                             , .waiting_copilot = PTHREAD_COND_INITIALIZER
                             , .waiting_second_kid = PTHREAD_COND_INITIALIZER

                             , .boat_ready = false
                             };

    struct molokai molokai = { .name = MOLOKAI
                             , .boat = NULL
                             , .mutex_molokai = PTHREAD_MUTEX_INITIALIZER
                             , .waiting_boat = PTHREAD_COND_INITIALIZER
                             , .first_gone = false
                             };

    struct boat    boat    = { .name = BOAT

                             , .parked = &oahu
                             , .oahu = &oahu
                             , .molokai = &molokai

                             , .pilot = false
                             , .copilot = false

                             , .rowing = false

                             , .mutex_boat = PTHREAD_MUTEX_INITIALIZER
                             , .cond_boat = PTHREAD_COND_INITIALIZER
                             , .cond_boat2 = PTHREAD_COND_INITIALIZER
                             };
    oahu.boat = &boat;

    // used for comunicate between the people and glud
    bool glud_say_finished = false;
    bool finished = false;
    pthread_mutex_t mutex_glud = PTHREAD_MUTEX_INITIALIZER;
    pthread_cond_t people_wait_glud = PTHREAD_COND_INITIALIZER;
    pthread_cond_t glud_wait_kids = PTHREAD_COND_INITIALIZER;

    struct glud_communication glud_com = {
          .glud_say_finished = &glud_say_finished
        , .finished = &finished
        , .mutex_glud = &mutex_glud
        , .people_wait_glud = &people_wait_glud
        , .glud_wait_kids = &glud_wait_kids
    };

    // creating all the people (threads)
    int rc, i;
    for (i=0; i<KIDS_OAHU; ++i) {

        printf("glud: creating citizen %d kid in Oahu\n", i);
        people_state[i].place = &oahu;
        people_state[i].glud_com = &glud_com;

        rc = pthread_create(&people[i], NULL, kid, (void *) &people_state[i]);
        assert(0 == rc);

    } for (; i<TOTAL_OAHU; ++i) {

        printf("glud: creating citizen %d adult in Oahu\n", i);
        people_state[i].place = &oahu;
        people_state[i].glud_com = &glud_com;

        rc = pthread_create(&people[i], NULL, adult, (void *) &people_state[i]);
        assert(0 == rc);

    } for (; i<TOTAL_OAHU + KIDS_MOLOKAI; ++i) {

        printf("glud: creating citizen %d kid in Molokai\n", i);
        people_state[i].place = &molokai;
        people_state[i].glud_com = &glud_com;

        rc = pthread_create(&people[i], NULL, kid, (void *) &people_state[i]);
        assert(0 == rc);

    } for (; i<TOTAL_PEOPLE; ++i) {

        printf("glud: creating citizen %d adult in Molokai\n", i);
        people_state[i].place = &molokai;
        people_state[i].glud_com = &glud_com;

        rc = pthread_create(&people[i], NULL, adult, (void *) &people_state[i]);
        assert(0 == rc);

    }

    pthread_mutex_lock( &mutex_glud );
    // while I (Glud) not said that everything is ok, then
    while (!glud_say_finished) {
        // if the kids said that they finished, then check if true
        if(finished) {
            glud_say_finished = true;
            // checking if all the people are in Molokai
            // if not, Glud say that everything is not okay
            for(i=0; i<TOTAL_PEOPLE; ++i)
                if( *((int *) people_state[i].place ) != MOLOKAI ) {
                    glud_say_finished = false;
                    finished = false;
                    break;
                }
            if(glud_say_finished)
                printf("glud say: all finished\n");

            // say to all the people who are waiting the aswer of Glud that now
            // Glud has decided
            pthread_cond_broadcast( &people_wait_glud );
        }
        else
            // if the kids have not said anything then wait for they
            pthread_cond_wait( &glud_wait_kids, &mutex_glud );
    }
    pthread_mutex_unlock( &mutex_glud );

    /* wait for all threads to complete */
    for (i=0; i<TOTAL_PEOPLE; ++i) {
       rc = pthread_join(people[i], NULL);
       assert(0 == rc);
    }

    printf("*** simulation finalized with success ***\n");

    exit(EXIT_SUCCESS);
}
