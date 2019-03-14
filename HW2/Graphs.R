count_private <- table(college$Private)

barplot(count_private, main="Private vs Non-Private University Counts", xlab= "Number of Private vs. Non-Private Universities", names.arg=c("Non-Private", "Private"), col=c("darkblue","red"))

plot(college$Apps, college$Accept, main="Number of Applications vs. Number of Students Accepted", xlab="Number of Applications", ylab="Number of Students Accepted", pch=19)

boxplot(college$Enroll,  main= "Spread of College Enrollment Numbers", horizontal=TRUE, xlab="Number of Students Enrolled")

