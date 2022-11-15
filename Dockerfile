ARG  BASE_IMAGE
FROM ${BASE_IMAGE}
RUN apt update && apt install -y git make
RUN groupadd epigrapher \
    --gid 1201 \
    && useradd epigrapher \
    --create-home \
    --gid 1201 \
    --shell /bin/bash \
    --uid 1200 \
    && usermod -a -G sudo epigrapher \
    && echo 'ALL ALL = (ALL) NOPASSWD: ALL' >> /etc/sudoers \
    && echo 'epigrapher:secret' | chpasswd
ENV HOME=/home/epigrapher

USER epigrapher
COPY prolog/ /app
ARG PORT
ENV PORT=$PORT
CMD app/prerequisites.pl && swipl app/epigrapher.pl -g server --port=${PORT} --no-fork --debug='http(request)'
EXPOSE ${PORT}